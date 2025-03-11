package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.AmuMmaIO

class Mma(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits
  
  protected val mtilem = in.data.src(0)
  protected val mtilen = in.data.src(1)
  protected val mtilek = in.data.src(2)

  val output = Wire(new AmuMmaIO)
  output := 0.U.asTypeOf(new AmuMmaIO)
  output.mtilem := mtilem
  output.mtilen := mtilen
  output.mtilek := mtilek
  
  out.res.data := 0.U
}