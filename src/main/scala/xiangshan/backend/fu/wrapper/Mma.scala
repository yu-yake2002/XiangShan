package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.{AmuMmaIO, MtypeMSew}
import xiangshan.MmulOpType

class Mma(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val mtilem = in.data.src(2)
  protected val mtilen = in.data.src(3)
  protected val mtilek = in.data.src(4)

  dontTouch(in.data.imm)

  val output = Wire(new AmuMmaIO)
  dontTouch(output)
  output.ms1    := in.data.imm(7, 4)
  output.ms2    := in.data.imm(11, 8)
  output.md     := in.data.imm(3, 0)
  output.widths := MmulOpType.getFromType(in.ctrl.fuOpType)
  output.widthd := MmulOpType.getToType(in.ctrl.fuOpType)
  output.sat    := false.B
  output.mtilem := mtilem
  output.mtilen := mtilen
  output.mtilek := mtilek
  
  out.res.data := output.asUInt
}