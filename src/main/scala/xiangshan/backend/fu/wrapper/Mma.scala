package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
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

  protected val realFuOpType = WireInit(in.ctrl.fuOpType)
  when(MmulOpType.isFromMsew(in.ctrl.fuOpType)) {
    realFuOpType := Cat(in.ctrl.fuOpType(8, 6), in.ctrl.mpu.get.mtype.msew, in.ctrl.fuOpType(2, 0))
  }

  val output = Wire(new AmuMmaIO)
  dontTouch(output)
  output.ms1    := in.data.imm(7, 4)
  output.ms2    := in.data.imm(11, 8)
  output.md     := in.data.imm(3, 0)

  output.types  := MmulOpType.getFromType(realFuOpType)
  output.typed  := MmulOpType.getToType(realFuOpType)
  
  output.sat    := MmulOpType.isSat(realFuOpType)
  output.mtilem := mtilem
  output.mtilen := mtilen
  output.mtilek := mtilek
  
  out.res.data := output.asUInt
}