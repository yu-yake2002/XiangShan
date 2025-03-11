package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.AmuLsuIO

class Mlsu(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val baseAddress = in.data.src(0)
  protected val stride = in.data.src(1)
  protected val mtile2 = in.data.src(2)
  protected val mtile3 = in.data.src(3)

  val output = Wire(new AmuLsuIO)
  output := 0.U.asTypeOf(new AmuLsuIO)
  
  out.res.data := 0.U
}