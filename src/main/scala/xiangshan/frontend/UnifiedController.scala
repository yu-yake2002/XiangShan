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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

trait UnifiedControllerParams extends HasXSParameter {
  val warmup: Int = RlControllerWarmup
  val coef: Int = RlControllerCoef
  val gamma: Int = RlControllerGamma
}

class UnifiedController(implicit p: Parameters) extends XSModule with UnifiedControllerParams{
  val io = IO(new Bundle {
    val newCommits: UInt = Input(UInt(3.W))
    val gates: Vec[Bool] = Output(Vec(2, Bool()))
  })

  // cycles and commits in this interval
  val cyclesCounter: UInt = RegInit(1.U(10.W))
  cyclesCounter := cyclesCounter + 1.U
  val commitsCounter: UInt = RegInit(0.U(13.W))

  // indicator of update
  val update: Bool = Wire(Bool())
  update := cyclesCounter === 0.U

  // commits in the last interval, used in log and sqrt calculation
  val commitsUpdate: UInt = RegEnable(commitsCounter, update)

  when (cyclesCounter === 0.U) {
    commitsCounter := io.newCommits
  }.otherwise {
    commitsCounter := commitsCounter + io.newCommits
  }

  /**
   * A simple combinatorial logic to calc log
   */
  class LogarithmCalculator extends Module {
    val io = IO(new Bundle {
      val nTotal: UInt = Input(UInt(8.W))
      val logResult: UInt = Output(UInt(8.W))
    })

    // Segment intervals
    val segments: Array[(UInt, UInt)] = Array(
      // TODO: fill me!
      // (upper limit, approximate value)
      (0.U, 0.U),
      (64.U, 20.U),
      (128.U, 40.U),
      (192.U, 60.U),
      (255.U, 80.U)
    )

    val inputInRange: Bool = WireDefault(false.B)
    val approxLog: UInt = Wire(UInt(8.W))

    // check each segment
    for ((upperLimit, approx) <- segments) {
      when(io.nTotal < upperLimit) {
        inputInRange := true.B
        approxLog := approx
      }
    }

    // If input is in segment, directly output corresponding value.
    // Otherwise output maximum value.
    io.logResult := Mux(inputInRange, approxLog, segments.last._2)
  }

  /**
   * A simple combinatorial logic to calc sqrt
   */
  class SquareRootCalculator extends Module {
    val io = IO(new Bundle {
      val input: UInt = Input(UInt(8.W))
      val sqrtResult: UInt = Output(UInt(8.W))
    })

    // Segment intervals
    val segments = Array(
      // TODO: fill me!
      // (upper limit, approximate value)
      (0.U, 0.U),
      (64.U, 8.U),
      (128.U, 11.U),
      (192.U, 14.U),
      (255.U, 16.U)
    )

    val inputInRange: Bool = WireDefault(false.B)
    val approxSqrt: UInt = Wire(UInt(8.W))

    // check each segment
    for ((upperLimit, approx) <- segments) {
      when(io.input <= upperLimit) {
        inputInRange := true.B
        approxSqrt := approx
      }
    }

    // If input is in segment, directly output corresponding value.
    // Otherwise output maximum value.
    io.sqrtResult := Mux(inputInRange, approxSqrt, segments.last._2)
  }

  class PotentialCalculator extends Module {
    val io = IO(new Bundle {
      val input = Flipped(DecoupledIO(UInt(8.W)))
      val output = DecoupledIO(UInt(8.W))
    })
    // TODO: calculate potential
    io.input.ready := false.B
    io.output.valid := false.B
    io.output.bits := DontCare
  }

  val pCalc: PotentialCalculator = Module(new PotentialCalculator())
  pCalc.io.input.valid := false.B
  pCalc.io.input.bits := DontCare
  pCalc.io.output.ready := false.B

  val updatedArm: UInt = RegInit(0.U(2.W))
  when (update === true.B) {
    updatedArm := 0.U
  }.elsewhen (updatedArm =/= 3.U) {
    updatedArm := updatedArm + 1.U
  }

  // TODO: update rewards of arms
  def selectArm(): UInt = {
    // TODO: select optical arm
    0.U
  }

  // Enable all gates
  val selectedArm: UInt = RegInit(0.U(2.W))

  val warmupRound: UInt = RegInit(0.U(3.W))
  val finishWarmup: Bool = Wire(Bool())
  finishWarmup := warmupRound === warmup.U
  // Update trainRound and selectedArm
  when (finishWarmup) {
    selectedArm := selectArm()
  }.otherwise {
    // Warmup hasn't been finished
    selectedArm := selectedArm + 1.U
    when (selectedArm === 3.U) {
      warmupRound := warmupRound + 1.U
    }
  }

  // transform arm to gates
  for (i <- 0 until 2) {
    io.gates(i) := selectedArm(i)
  }
}