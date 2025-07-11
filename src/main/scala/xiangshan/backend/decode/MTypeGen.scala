package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{MsetMtilexModule, MsetMtypeModule}
import xiangshan.backend.fu.matrix.Bundles.{MType, MsetMType}
import xiangshan.backend.decode.isa.bitfield.{InstMType, Riscv32BitInst, XSInstBitFields}

class MTypeGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchMType = Input(Bool())
    val walkMType   = Flipped(Valid(new MType))
    val canUpdateMType = Input(Bool())
    val mtype = Output(new MType)
    val msettypeMType = Input(new MType)
    val commitMType = new Bundle {
      val mtype = Flipped(Valid(new MType))
      val hasMsettype = Input(Bool())
    }
  })
  private val instValidVec = io.insts.map(_.valid)
  private val instFieldVec = io.insts.map(_.bits.asTypeOf(new XSInstBitFields))
  private val isMsettypeVec = VecInit(instFieldVec.map(field => (
    (field.OPCODE === "b1110111".U) && (field.ALL(31, 26) === "b000000".U) && (
      field.ALL(14) === "b1".U)
  )).zip(instValidVec).map { case (isMsettype, valid) => valid && isMsettype })

  private val firstMsetOH: Vec[Bool] = VecInit(PriorityEncoderOH(isMsettypeVec))
  private val firstMsetInstField: XSInstBitFields = PriorityMux(firstMsetOH, instFieldVec)

  private val isMsetmtilexi = (firstMsetInstField.OPCODE === "b1110111".U) && 
    (firstMsetInstField.ALL(31, 25) === "b0000011".U) && 
    (firstMsetInstField.ALL(14) === "b1".U)

  private val mtypeArch = RegInit(MType.initMtype())
  private val mtypeSpec = RegInit(MType.initMtype())

  private val mtypeArchNext = WireInit(mtypeArch)
  private val mtypeSpecNext = WireInit(mtypeSpec)

  mtypeArch := mtypeArchNext
  mtypeSpec := mtypeSpecNext

  private val instMType: InstMType = firstMsetInstField.ZIMM_MSETMTILEXI.asTypeOf(new InstMType)
  private val mtypei: MsetMType = MsetMType.fromInstMType(instMType)
  // TODO: use correct mtype and mask
  private val msettypeModule = Module(new MsetMtypeModule)
  msettypeModule.io.in.oldmtype := 0.U.asTypeOf(new MsetMType)
  msettypeModule.io.in.newmtype := mtypei
  msettypeModule.io.in.mask := 0.U
  private val mtypeNew = msettypeModule.io.out.mtype

  when(io.commitMType.hasMsettype) {
    mtypeArchNext := io.msettypeMType
  }.elsewhen(io.commitMType.mtype.valid) {
    mtypeArchNext := io.commitMType.mtype.bits
  }

  private val inHasMsettype = isMsettypeVec.asUInt.orR

  when(io.commitMType.hasMsettype) {
    // when msettype instruction commit, also update mtypeSpec, because msettype flush pipe
    mtypeSpecNext := io.msettypeMType
  }.elsewhen(io.walkMType.valid) {
    mtypeSpecNext := io.walkMType.bits
  }.elsewhen(io.walkToArchMType) {
    mtypeSpecNext := mtypeArch
  }.elsewhen(inHasMsettype && io.canUpdateMType) {
    mtypeSpecNext := mtypeNew
  }

  io.mtype := mtypeSpec

  // just make verilog more readable
  dontTouch(isMsettypeVec)
}