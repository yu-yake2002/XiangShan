package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.{AmuCtrlIO, AmuReleaseIO}

class Mrelease(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  val output = Wire(new AmuReleaseIO)
  output.tokenRd := in.data.imm

  out.res.data := 0.U
  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.releaseOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
}
