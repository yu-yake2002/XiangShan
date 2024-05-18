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
import utils.XSPerfHistogram
import xiangshan._

trait UnifiedControllerParams extends HasXSParameter {
  def warmup: Int = RlControllerWarmup
  def coef: Int = RlControllerCoef
  def gamma: Int = RlControllerGamma

  def intWidth: Int = 12
  def fracWidth: Int = 12
  def totWidth: Int = intWidth + fracWidth
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
    val frac: Double = value - value.toInt
    val fracBits: Int = (frac * Math.pow(2, fracWidth)).toInt
    fixedPointValue(value.toInt.U(intWidth.W), fracBits.U(fracWidth.W))
  }

  /**
   * A simple combinatorial logic to calc log
   */
  class LogarithmCalculator extends XSModule with UnifiedControllerParams{
    val io = IO(new Bundle {
      val input = Flipped(ValidIO(new FixedPoint))
      val logResult: FixedPoint = Output(new FixedPoint)
    })
    assert(!(io.input.valid && io.input.bits.sign === true.B), "Negative number is invalid.")
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
    assert(!(io.input.valid && io.input.bits.sign === true.B), "Negative number is invalid.")
    io.sqrtResult.sign := false.B

    // Segment intervals
    val segments: IndexedSeq[(UInt, UInt)] = for (i <- -12 to 2) yield {
      // (upper limit, approximate value)
      (fixedPointValue(scala.math.pow(2, i)), fixedPointValue(scala.math.pow(2, i / 2.0)))
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
    assert(!(io.in.valid && io.in.bits.divisor.value === 0.U), "ArithmeticException!")

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
  val rewardStep: UInt = Cat(0.U((intWidth + 10 - commitsCounter.getWidth).W), commitsCounter, 0.U((fracWidth - 10).W))
  val rewardStepReg: UInt = RegEnable(rewardStep, 0.U, update)

  // TODO: How many bits are needed here?
  val selectedArm: UInt = RegInit(0.U(2.W))
  val updatingArm: UInt = RegInit(0.U(2.W))

  val rewardsReg: Vec[FixedPoint] = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new FixedPoint))))
  val rewardsWritePtr: UInt = selectedArm
  val rewardsWriteData: FixedPoint = WireDefault((0.U).asTypeOf(new FixedPoint))
  dontTouch(rewardsWriteData)
  val rewardsWriteEnable: Bool = WireDefault(false.B)
  when (rewardsWriteEnable) {
    rewardsReg(rewardsWritePtr) := rewardsWriteData
  }

  val niReg: Vec[FixedPoint] = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new FixedPoint))))
  val nTotalReg: FixedPoint = RegInit(0.U.asTypeOf(new FixedPoint))

  val potentialReg: Vec[FixedPoint] = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new FixedPoint))))
  val potentialWritePtr: UInt = updatingArm
  val potentialWriteData: FixedPoint = WireDefault((0.U).asTypeOf(new FixedPoint))
  val potentialWriteEnable: Bool = WireDefault(false.B)
  when (potentialWriteEnable) {
    potentialReg(potentialWritePtr) := potentialWriteData
  }

  val (warmUp :: warmUpSendUpdRew :: warmUpWaitUpdRew :: pendingState :: sendUpdRew :: waitUpdRew :: sendNextArm ::
    waitNextArm :: updSels :: Nil) = Enum(9)
  val state: UInt = RegInit(warmUp)
  val warmupRound: UInt = RegInit(0.U(3.W))
  val finishWarmup: Bool = warmupRound === warmup.U

  val selectedArmReward: FixedPoint = WireDefault(rewardsReg(selectedArm))
  dontTouch(rewardsReg)

  def findMaxPotentialArm(potential: Vec[FixedPoint]): UInt = {
    class findMaxBundle extends Bundle {
      val fp = new FixedPoint
      val idx: UInt = UInt((potential.size + 1).W)
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
    findMax(0, potential.size).idx(1, 0)
  }

  switch(state) {
    is(warmUp) {
      when(update) {
        when(!finishWarmup && selectedArm === 3.U) {
          warmupRound := warmupRound + 1.U
        }
        when(warmupRound === 0.U) {
          // Don't wait for divider.
          state := warmUp
          rewardsWriteEnable := true.B
          rewardsWriteData.value := rewardStep
          selectedArm := selectedArm + 1.U
          nTotalReg.value := nTotalReg.value + fixedPointValue(1)
          niReg(selectedArm).value := niReg(selectedArm).value + fixedPointValue(1)
        }.elsewhen(!finishWarmup) {
          // Wait for divider.
          state := warmUpSendUpdRew
        }.otherwise { // finish warmup
          state := pendingState
        }
      }
    }
    is(warmUpSendUpdRew) {
      when(divider.io.in.fire) {
        state := warmUpWaitUpdRew
      }
    }
    is(warmUpWaitUpdRew) {
      when(divider.io.result.fire) {
        when(finishWarmup) {
          state := sendNextArm
        }.otherwise {
          state := warmUp
        }
        rewardsWriteEnable := true.B
        rewardsWriteData.value := Mux(divider.io.result.bits.sign,
          selectedArmReward.value - divider.io.result.bits.value,
          selectedArmReward.value + divider.io.result.bits.value
        )
        selectedArm := selectedArm + 1.U
        niReg(selectedArm).value := niReg(selectedArm).value + fixedPointValue(1)
        nTotalReg.value := nTotalReg.value + fixedPointValue(1)
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
        rewardsWriteEnable := true.B
        rewardsWriteData.value := Mux(divider.io.result.bits.sign,
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
        potentialWriteEnable := true.B
        potentialWriteData.value := rewardsReg(potentialWritePtr).value.asUInt + sqrtTable.io.sqrtResult.value.asUInt
        when(updatingArm === 3.U) {
          state := updSels
        }.otherwise {
          state := sendNextArm
        }
        updatingArm := updatingArm + 1.U
      }
    }
    is(updSels) {
      state := pendingState
      val maxPot = findMaxPotentialArm(potentialReg)
      // dontTouch(maxPot)
      selectedArm := maxPot
      nTotalReg.value := (nTotalReg.value - (nTotalReg.value >> 7.U).asUInt + fixedPointValue(1))
      for (i <- 0 until 4) {
        when (i.U === maxPot) {
          niReg(i).value := niReg(i).value + fixedPointValue(1)
        }.otherwise {
          niReg(i).value := niReg(i).value - (niReg(i).value >> 7.U).asUInt
        }
      }
    }
  }

  lnTable.io.input.valid := state === sendNextArm
  lnTable.io.input.bits := nTotalReg
  sqrtTable.io.input.valid := divider.io.result.fire && (state === waitNextArm)
  sqrtTable.io.input.bits := divider.io.result.bits

  divider.io.in.valid := (((state === sendUpdRew) || (state === sendNextArm) || (state === warmUpSendUpdRew))
    && warmupRound =/= 0.U)
  val dividend: FixedPoint = Wire(new FixedPoint)
  dividend.sign := Mux((state === sendUpdRew) || (state === warmUpSendUpdRew),
    rewardStepReg < selectedArmReward.value,
    lnTable.io.logResult.sign
  )
  dividend.value := Mux((state === sendUpdRew) || (state === warmUpSendUpdRew),
    Mux(dividend.sign, selectedArmReward.value, rewardStepReg) - Mux(dividend.sign, rewardStepReg, selectedArmReward.value),
    lnTable.io.logResult.value
  )
  divider.io.in.bits.dividend := dividend
//  divider.io.in.bits.divisor := Mux((state === sendUpdRew) || (state === warmUpSendUpdRew),
//    niReg(updatingArm),
//    nTotalReg
//  )
  divider.io.in.bits.divisor := niReg(updatingArm)
  divider.io.result.ready := (state === waitUpdRew) || (state === waitNextArm) || (state === warmUpWaitUpdRew)

  // transform arm to gates
//  for (i <- 0 until 2) {
//    io.gates(i) := selectedArm(i)
//  }
   for (i <- 0 until 2) {
     io.gates(i) := true.B
   }
  XSPerfHistogram("ftbCtrl_select", selectedArm, true.B, 0, 3, 1)
}