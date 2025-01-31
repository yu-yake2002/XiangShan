package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VType, VsetVType}
import xiangshan.backend.decode.isa.bitfield.{InstVType, Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.fu.VsetModule
import xiangshan.backend.fu.matrix.Bundles.MsetMType

class VTypeGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchVType = Input(Bool())
    val walkVType   = Flipped(Valid(new VType))
    val canUpdateVType = Input(Bool())
    val vtype = Output(new VType)
    val vsetvlVType = Input(new VType)
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
  })
  private val instValidVec = io.insts.map(_.valid)
  private val instFieldVec = io.insts.map(_.bits.asTypeOf(new XSInstBitFields))
  // Only check vsetvli and vsetivli here.
  // vsetvl will flush pipe, need not to generate new vtype in decode stage.
  private val isVsetVec = VecInit(instFieldVec.map(fields =>
    (fields.OPCODE === "b1010111".U) && (fields.WIDTH === "b111".U) && (
      fields.ALL(31) === "b0".U ||
      fields.ALL(31, 30) === "b11".U
    )
  ).zip(instValidVec).map { case (isVset, valid) => valid && isVset})

  private val firstVsetOH: Vec[Bool] = VecInit(PriorityEncoderOH(isVsetVec))
  private val firstVsetInstField: XSInstBitFields = PriorityMux(firstVsetOH, instFieldVec)

  private val isVsetvli= (firstVsetInstField.OPCODE === "b1010111".U) && 
    (firstVsetInstField.WIDTH === "b111".U) && 
    (firstVsetInstField.ALL(31) === "b0".U)

  private val vtypeArch = RegInit(VType.initVtype())
  private val vtypeSpec = RegInit(VType.initVtype())

  private val vtypeArchNext = WireInit(vtypeArch)
  private val vtypeSpecNext = WireInit(vtypeSpec)

  vtypeArch := vtypeArchNext
  vtypeSpec := vtypeSpecNext

  private val instVType: InstVType = Mux(isVsetvli, firstVsetInstField.ZIMM_VSETVLI.asTypeOf(new InstVType), 
    firstVsetInstField.ZIMM_VSETIVLI.asTypeOf(new InstVType))
  private val vtypei: VsetVType = VsetVType.fromInstVType(instVType)

  private val vsetModule = Module(new VsetModule)
  vsetModule.io.in.avl := 0.U
  vsetModule.io.in.vtype := vtypei
  vsetModule.io.in.func := VSETOpType.uvsetvcfg_xi

  private val vtypeNew = vsetModule.io.out.vconfig.vtype

  when(io.commitVType.hasVsetvl) {
    vtypeArchNext := io.vsetvlVType
  }.elsewhen(io.commitVType.vtype.valid) {
    vtypeArchNext := io.commitVType.vtype.bits
  }

  private val inHasVset = isVsetVec.asUInt.orR

  when(io.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    vtypeSpecNext := io.vsetvlVType
  }.elsewhen(io.walkVType.valid) {
    vtypeSpecNext := io.walkVType.bits
  }.elsewhen(io.walkToArchVType) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(inHasVset && io.canUpdateVType) {
    vtypeSpecNext := vtypeNew
  }

  io.vtype := vtypeSpec

  // just make verilog more readable
  dontTouch(isVsetVec)
}