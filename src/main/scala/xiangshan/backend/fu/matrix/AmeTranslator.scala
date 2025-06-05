package xiangshan.backend.fu.matrix

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Expander._
import xiangshan.backend.fu.matrix.Bundles._

class AmeTranslatorIO(implicit p: Parameters) extends Bundle {
  val amuCtrl: DecoupledIO[AmuCtrlIO] = Flipped(Decoupled(new AmuCtrlIO))
  val uop = Flipped(new Uop_IO)
  val mlu_l2 = Flipped(new MLU_L2_IO)
}

class AmeTranslator(implicit p: Parameters) extends Module {
  val io = IO(new AmeTranslatorIO)

  val mem_port_length = io.mlu_l2.Cacheline_Read_io.length
  val id_width = io.mlu_l2.Cacheline_Read_io.head.id.getWidth

  // Id reg vec to record id from mlu_l2_io.read
  val idRegVec = RegInit(VecInit(Seq.fill(mem_port_length)(0.U(id_width.W))))
  val respValidVec = RegInit(VecInit(Seq.fill(mem_port_length)(false.B)))

  // Default values
  io.uop.ShakeHands_io.valid := false.B
  io.uop.Operands_io := 0.U.asTypeOf(new Operands_IO)
  io.uop.InsType_io := 0.U.asTypeOf(new InsType_IO)
  io.uop.mtileConfig_io := 0.U.asTypeOf(new mtileConfig_IO)
  io.amuCtrl.ready := io.uop.ShakeHands_io.ready
  io.mlu_l2.Cacheline_ReadBack_io.foreach { e =>
    e.valid := false.B
    e.data := 0.U
    e.id := 0.U
  }

  /** Decode the control signals */
  when(io.amuCtrl.valid) {
    io.uop.ShakeHands_io.valid := true.B

    when(io.amuCtrl.bits.isMma()) {
      // MMA operation
      val mmaio = io.amuCtrl.bits.data.asTypeOf(new AmuMmaIO)

      // Map MMA fields to Uop_io
      io.uop.Operands_io.ms1 := mmaio.ms1
      io.uop.Operands_io.ms2 := mmaio.ms2
      io.uop.Operands_io.md := mmaio.md

      // Set instruction type
      io.uop.InsType_io.is_mmacc := true.B
      io.uop.InsType_io.is_mlbe8 := false.B

      // Set tile configuration
      io.uop.mtileConfig_io.mtilem := mmaio.mtilem
      io.uop.mtileConfig_io.mtilen := mmaio.mtilen
      io.uop.mtileConfig_io.mtilek := mmaio.mtilek

    }.elsewhen(io.amuCtrl.bits.isMls()) {
      // Load/Store operation
      val lsuio = io.amuCtrl.bits.data.asTypeOf(new AmuLsuIO)

      // Map LSU fields to Uop_io
      io.uop.Operands_io.md := lsuio.ms
      io.uop.Operands_io.rs1 := lsuio.baseAddr
      io.uop.Operands_io.rs2 := lsuio.stride

      // Set instruction type
      io.uop.InsType_io.is_mlbe8 := !lsuio.ls // is_mlbe8 is true for load operations

      // Set tile configuration
      io.uop.mtileConfig_io.mtilem := lsuio.row
      io.uop.mtileConfig_io.mtilen := lsuio.column
      io.uop.mtileConfig_io.mtilek := 0.U // Not used for load/store operations
    }
  }

  /** mlu_l2_io logic */
  for (i <- 0 until mem_port_length) {
    when(io.mlu_l2.Cacheline_Read_io(i).valid) {
      idRegVec(i) := io.mlu_l2.Cacheline_Read_io(i).id
      respValidVec(i) := true.B
    }
  }

  for (i <- 0 until mem_port_length) {
    when(respValidVec(i)) {
      io.mlu_l2.Cacheline_ReadBack_io(i).valid := true.B
      io.mlu_l2.Cacheline_ReadBack_io(i).id := idRegVec(i)
      // Note: The following value is just for smoke testing
      io.mlu_l2.Cacheline_ReadBack_io(i).data := "hdeadbeef0000".U | i.U
    }
  }
}