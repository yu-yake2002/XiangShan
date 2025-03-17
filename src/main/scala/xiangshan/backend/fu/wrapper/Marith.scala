package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}

class Marith(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val mtilex0 = in.data.src(0)
  protected val mtilex1 = in.data.src(1)

  out.res.data := 0.U

  out.ctrl.amuCtrl.get.data := 0.U
}