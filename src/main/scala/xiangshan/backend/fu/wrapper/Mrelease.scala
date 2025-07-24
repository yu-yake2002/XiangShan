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
  dontTouch(output)
  output.tokenRd := in.data.src(0)

  out.res.data := 0.U.asTypeOf(out.res.data)

  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.releaseOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
}
