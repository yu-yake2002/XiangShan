package xiangshan.backend.fu.matrix

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles._
import hbl2demo.{AMUIO, RegInfo}
import coupledL2.MatrixDataBundle

class AmuCtrlTranslator(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ctrl = Flipped(DecoupledIO(new AmuCtrlIO))
    val amu = Flipped(new AMUIO)
  })

  // Default values
  io.amu.init_fire := false.B
  io.amu.ld_fire := false.B
  io.amu.st_fire := false.B
  io.amu.reg_in := 0.U.asTypeOf(new RegInfo)

  io.ctrl.ready := true.B

  io.amu.matrix_data_in.foreach { data =>
    data.valid := false.B
    data.bits := 0.U.asTypeOf(new MatrixDataBundle)
  }

  io.amu.reg_in := 0.U.asTypeOf(new RegInfo)

  // Decode the control signals
  when(io.ctrl.valid) {
    when(io.ctrl.bits.isMma()) {
      // MMA operation
      io.amu.init_fire := true.B
      // TODO: Set reg_in based on MMA operation data
      io.amu.reg_in := io.ctrl.bits.data.asTypeOf(new RegInfo)
    }.elsewhen(io.ctrl.bits.isMls()) {
      // Load/Store operation
      val lsuCtrl = io.ctrl.bits.data.asTypeOf(new AmuLsuIO)
      // Set load/store fire signals
      io.amu.ld_fire := !lsuCtrl.ls 
      io.amu.st_fire := lsuCtrl.ls
    }
  }
} 