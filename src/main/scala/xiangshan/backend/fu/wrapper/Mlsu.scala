package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.AmuLsuIO
import xiangshan.MldstOpType

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
  dontTouch(output)
  output := 0.U.asTypeOf(new AmuLsuIO)
  output.ls := MldstOpType.isStore(in.ctrl.fuOpType)
  output.ms := in.data.imm(3, 0)
  output.widths := Mux1H(Seq(
    MldstOpType.isFp8(in.ctrl.fuOpType)  -> 3.U,
    MldstOpType.isFp16(in.ctrl.fuOpType) -> 4.U,
    MldstOpType.isFp32(in.ctrl.fuOpType) -> 5.U,
  ))
  output.baseAddr := baseAddress
  output.stride := stride
  output.transpose := MldstOpType.isTransposed(in.ctrl.fuOpType)
  output.row := mtile2
  output.column := mtile3
  
  out.res.data := output.asUInt
}