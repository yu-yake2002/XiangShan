/***************************************************************************************
 * Copyright (c) 2024 Nanjing University
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.frontend

import chisel3.{Vec, _}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

trait UnifiedControllerParams extends HasXSParameter {
  val warmup: Int = RlControllerWarmup
  val coef: Int = RlControllerCoef
  val gamma: Int = RlControllerGamma

  val intWidth: Int = 12
  val fracWidth: Int = 12
}

class UnifiedController(implicit p: Parameters) extends XSModule with UnifiedControllerParams{
  val io = IO(new Bundle {
    val newCommits: UInt = Input(UInt(3.W))
    val gates: Vec[Bool] = Output(Vec(2, Bool()))
  })

  class FixedPoint(implicit p: Parameters) extends XSBundle with UnifiedControllerParams{
    // True for -. False for +.
    val sign = Bool()
    val value = UInt((intWidth + fracWidth).W)

    def >(o: FixedPoint): Bool = value > o.value
  }

  def fixedPointValue(intPart: UInt, fracPart: UInt): UInt = {
    assert(intPart.getWidth == intWidth && fracPart.getWidth == fracWidth, "Width mismatch!")
    Cat(intPart, fracPart)
  }

  def fixedPointValue(value: Double): UInt = {
    assert(value >= 0)
    var frac: Double = value - value.toInt
    val fracPart = for (i <- 1 to fracWidth) yield {
      val bit = frac * 2.0 >= 1.0
      frac *= 2.0
      if (bit) {
        frac -= 1.0
      }
      bit.B
    }
    fixedPointValue(value.toInt.U(intWidth.W), Reverse(VecInit(fracPart).asUInt))
  }

  /**
   * A simple combinatorial logic to calc log
   */
  class LogarithmCalculator extends XSModule with UnifiedControllerParams{
    val io = IO(new Bundle {
      val input = Flipped(ValidIO(new FixedPoint))
      val logResult: FixedPoint = Output(new FixedPoint)
    })
    // assert(!(io.input.valid && io.input.bits.sign === false.B), "Negative number is invalid.")
    val resultSign: Bool = !io.input.bits.value(intWidth + fracWidth - 1, fracWidth).orR
    io.logResult.sign := resultSign

    // Segment intervals
    val segments: Array[(UInt, UInt)] = Array(
      // (upper limit, approximate value)
      (fixedPointValue(1), fixedPointValue(0)),
      (fixedPointValue(4), fixedPointValue(1)),
      (fixedPointValue(12), fixedPointValue(2)),
      (fixedPointValue(33), fixedPointValue(3)),
      (fixedPointValue(90), fixedPointValue(4)),
      (fixedPointValue(244), fixedPointValue(5))
    )

    val inputInRange: Bool = io.input.bits.value < segments.last._1
    val approxResultSelector: UInt = PriorityEncoder(segments.map(io.input.bits.value < _._1))
    val approxResult: UInt = VecInit(segments.map(_._2))(approxResultSelector)

    // If input is in segment, directly output corresponding value.
    // Otherwise output maximum value.
    io.logResult.value := Mux(inputInRange, approxResult, segments.last._2)
  }

  /**
   * A simple combinatorial logic to calc sqrt
   */
  class SquareRootCalculator extends XSModule {
    val io = IO(new Bundle {
      val input = Flipped(ValidIO(new FixedPoint))
      val sqrtResult: FixedPoint = Output(new FixedPoint)
    })
    // assert(!(io.input.valid && io.input.bits.sign === false.B), "Negative number is invalid.")
    io.sqrtResult.sign := false.B

    // Segment intervals
    val segments: IndexedSeq[(UInt, UInt)] = for (i <- -6 to 1) yield {
      // (upper limit, approximate value)
      (fixedPointValue(scala.math.pow(4, i)), fixedPointValue(scala.math.pow(4, i / 2)))
    }

    val inputInRange: Bool = io.input.bits.value < segments.last._1
    val approxSqrtSelector: UInt = PriorityEncoder(segments.map(io.input.bits.value < _._1))
    val approxSqrt: UInt = VecInit(segments.map(_._2))(approxSqrtSelector)

    // If input is in segment, directly output corresponding value.
    // Otherwise output maximum value.
    io.sqrtResult.value := Mux(inputInRange, approxSqrt, segments.last._2)
  }

  class DividerIO(implicit p: Parameters) extends XSBundle {
    val dividend: FixedPoint = Input(new FixedPoint)
    val divisor: FixedPoint = Input(new FixedPoint)
  }

  class FixedPointDivider(implicit p: Parameters) extends XSModule with UnifiedControllerParams{
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(new DividerIO))
      val result = Decoupled(new FixedPoint)
    })
    // TODO: add me!
    // assert(!(io.in.valid && io.in.bits.divisor.value === 0.U), "ArithmeticException!")

    // Define states for the state machine
    val waitingForInput :: calculating :: waitingForResultAccept :: Nil = Enum(3)
    val state: UInt = RegInit(waitingForInput)

    // Sign
    io.result.bits.sign := RegEnable(io.in.bits.divisor.sign ^ io.in.bits.dividend.sign, io.in.fire)

    // Value
    // Define registers to hold the dividend and divisor
    val dividendReg: UInt = Reg(UInt((intWidth + (intWidth + fracWidth) + fracWidth).W))
    val divisorReg: UInt = RegEnable(io.in.bits.divisor.value, io.in.fire)
    val quotientReg: UInt = Reg(UInt((intWidth + fracWidth).W))

    // Counter for tracking the number of cycles
    val divCyclesCounter: UInt = RegInit(0.U(log2Ceil(intWidth + fracWidth + 1).W))
    val resultValid: Bool = divCyclesCounter === (intWidth + fracWidth - 1).U

    val subtracter: UInt = Wire(UInt((intWidth + (intWidth + fracWidth) + fracWidth).W))
    subtracter := divisorReg << ((intWidth + fracWidth - 1).U - divCyclesCounter)

    // Used bits in dividend for each iteration
    val dividendWire: Vec[UInt] = Wire(Vec(intWidth + fracWidth, UInt((intWidth + fracWidth + 1).W)))
    dividendWire.zipWithIndex.foreach{
      case(num, idx) => num := dividendReg((intWidth + fracWidth) * 2 - 1 - idx, intWidth + fracWidth - 1 - idx)
    }

    // Main division logic
    switch(state) {
      is(waitingForInput) {
        when(io.in.fire) {
          state := calculating
          dividendReg := Cat(0.U(intWidth.W), io.in.bits.dividend.value, 0.U(fracWidth.W))
          quotientReg := 0.U
          divCyclesCounter := 0.U
        }
      }
      is(calculating) {
        // Subtract the divisor from the remainder
        when (dividendWire(divCyclesCounter) >= divisorReg) {
          dividendReg := dividendReg - subtracter
          val tmpBools = VecInit(quotientReg.asBools)
          tmpBools((intWidth + fracWidth - 1).U - divCyclesCounter) := true.B
          quotientReg := tmpBools.asUInt
        }

        // Increment cycle counter
        divCyclesCounter := divCyclesCounter + 1.U

        when(resultValid) {
          state := waitingForResultAccept
        }
      }
      is(waitingForResultAccept) {
        when(io.result.fire) {
          state := waitingForInput
        }
      }
    }

    // Output the result
    io.result.valid := state === waitingForResultAccept
    io.result.bits.value := quotientReg
    io.in.ready := state === waitingForInput
  }

  val lnTable = Module(new LogarithmCalculator)
  val sqrtTable = Module(new SquareRootCalculator)
  val divider = Module(new FixedPointDivider)

  // cycles and commits in this interval(1024 cycles)
  val cyclesCounter: UInt = RegInit(1.U(10.W))
  cyclesCounter := cyclesCounter + 1.U
  val update: Bool = cyclesCounter === 0.U
  val commitsCounter: UInt = RegInit(0.U(13.W))
  commitsCounter := Mux(update, 0.U, commitsCounter) + io.newCommits

  // commits in the last interval, used in calculation
  val commitsUpdate: UInt = RegEnable(commitsCounter, update)
  val rewardStep: UInt = Cat(0.U((intWidth + 10 - commitsUpdate.getWidth).W), commitsUpdate, 0.U((fracWidth - 10).W))

  val rewardsReg: Vec[FixedPoint] = Reg(Vec(4, new FixedPoint))
  val niReg: Vec[FixedPoint] = Reg(Vec(4, new FixedPoint))
  val nTotalReg: FixedPoint = Reg(new FixedPoint)
  val potentialReg: Vec[FixedPoint] = Reg(Vec(4, new FixedPoint))

  val warmUp :: pendingState :: sendUpdRew :: waitUpdRew :: sendNextArm :: waitNextArm :: updSels :: Nil = Enum(7)
  val state: UInt = RegInit(warmUp)
  val warmupRound: UInt = RegInit(0.U(3.W))
  val finishWarmup: Bool = warmupRound === warmup.U

  // TODO: How many bits are needed here?
  val selectedArm: UInt = RegInit(0.U(2.W))
  val updatingArm: UInt = RegInit(0.U(2.W))
  val selectedArmReward: FixedPoint = rewardsReg(selectedArm)

  def findMaxPotentialArm(potential: Vec[FixedPoint]): UInt = {
    class findMaxBundle extends Bundle {
      val fp = new FixedPoint
      val idx: UInt = UInt(potential.size.W)
    }
    object findMaxBundle {
      def apply(fp: FixedPoint, idx: UInt): findMaxBundle = {
        val res = Wire(new findMaxBundle)
        res.fp := fp
        res.idx := idx
        res
      }
    }

    def findMax(left: Int, right: Int): findMaxBundle = {
      if (left + 1 == right) {
        findMaxBundle(potential(left), left.U)
      } else {
        val mid = (left + right) / 2
        val leftResult = findMax(left, mid)
        val rightResult = findMax(mid, right)
        Mux(leftResult.fp > rightResult.fp, leftResult, rightResult)
      }
    }
    findMax(0, potential.size).idx
  }

  switch(state) {
    is(warmUp) {
      when (update) {
        when(selectedArm =/= 3.U) {
          // Simply select next arm.
          selectedArm := selectedArm + 1.U
        }.otherwise {
          // Finish a warmup round.
          when(!finishWarmup) {
            warmupRound := warmupRound + 1.U
            selectedArm := 0.U
          }.otherwise {
            state := pendingState
          }
        }
      }
    }
    is(pendingState) {
      when(update) {
        assert(finishWarmup)
        state := sendUpdRew
      }
    }
    is(sendUpdRew) {
      when(divider.io.in.fire) {
        state := waitUpdRew
      }
    }
    is(waitUpdRew) {
      when(divider.io.result.fire) {
        selectedArmReward.value := Mux(divider.io.result.bits.sign,
          selectedArmReward.value - divider.io.result.bits.value,
          selectedArmReward.value + divider.io.result.bits.value
        )
        state := sendNextArm
        updatingArm := 0.U
      }
    }
    is(sendNextArm) {
      when(divider.io.in.fire) {
        state := waitNextArm
      }
    }
    is(waitNextArm) {
      when(divider.io.result.fire) {
        when(updatingArm === 3.U) {
          state := updSels
          updatingArm := 0.U
        }.otherwise {
          state := sendNextArm
          updatingArm := updatingArm + 1.U
        }
      }
    }
    is(updSels) {
      when(updatingArm === 3.U) {
        state := pendingState
        selectedArm := findMaxPotentialArm(potentialReg)
      }
    }
  }

  lnTable.io.input.valid := state === sendNextArm
  lnTable.io.input.bits := nTotalReg
  sqrtTable.io.input.valid := false.B // TODO: implement me!
  sqrtTable.io.input.bits := divider.io.result.bits

  divider.io.in.valid := (state === sendUpdRew) || (state === sendNextArm)
  val dividend: FixedPoint = Wire(new FixedPoint)
  dividend.sign := Mux(state === sendUpdRew,
    rewardStep < selectedArmReward.value,
    lnTable.io.logResult.sign
  )
  dividend.value := Mux(state === sendUpdRew,
    Mux(dividend.sign, selectedArmReward.value, rewardStep) - Mux(dividend.sign, rewardStep, selectedArmReward.value),
    lnTable.io.logResult.value
  )
  divider.io.in.bits.dividend := dividend
  divider.io.in.bits.divisor := Mux(state === sendUpdRew,
    nTotalReg,
    niReg(updatingArm)
  )
  divider.io.result.ready := (state === waitUpdRew) || (state === waitNextArm)

  // transform arm to gates
//  for (i <- 0 until 2) {
//    io.gates(i) := selectedArm(i)
//  }
  for (i <- 0 until 2) {
    io.gates(i) := true.B
  }
}