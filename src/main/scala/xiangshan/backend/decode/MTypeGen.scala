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

  private val instMtype: UInt = firstMsetInstField.IMM_MSET
  private val instMfield: UInt = firstMsetInstField.IMM_MSETFIELD
  private val instMsetval: UInt = firstMsetInstField.IMM_MSETVAL
  private val instUse10bits = (firstMsetInstField.FUNCT3 === "b101".U || 
    firstMsetInstField.FUNCT3 === "b100".U)
  private val instMfieldOp: UInt = WireInit(0.U(8.W))
  switch(instMfield(3, 0)) {
    is ("b0000".U) { instMfieldOp := MSETtypeOpType.msetsew }
    is ("b0001".U) { instMfieldOp := MSETtypeOpType.msetint4 }
    is ("b0010".U) { instMfieldOp := MSETtypeOpType.msetint8 }
    is ("b0011".U) { instMfieldOp := MSETtypeOpType.msetint16 }
    is ("b0100".U) { instMfieldOp := MSETtypeOpType.msetint32 }
    is ("b0101".U) { instMfieldOp := MSETtypeOpType.msetint64 }
    is ("b0110".U) { instMfieldOp := MSETtypeOpType.msetfp8 }
    is ("b0111".U) { instMfieldOp := MSETtypeOpType.msetfp16 }
    is ("b1000".U) { instMfieldOp := MSETtypeOpType.msetfp32 }
    is ("b1001".U) { instMfieldOp := MSETtypeOpType.msetfp64 }
    is ("b1010".U) { instMfieldOp := MSETtypeOpType.msetba }
  }
  
  private val msettypeModule = Module(new MsetMtypeModule)
  msettypeModule.io.in.oldmtype := MType.toMsetMType(mtypeSpec)
  msettypeModule.io.in.newmtype := Mux(instUse10bits, Cat(instMsetval, instMfield), instMsetval)
  msettypeModule.io.in.func := 0.U
  switch (firstMsetInstField.FUNCT3) {
    is ("b100".U) { msettypeModule.io.in.func := MSETtypeOpType.msettypei }
    is ("b101".U) { msettypeModule.io.in.func := MSETtypeOpType.msettypehi }
    is ("b110".U) { msettypeModule.io.in.func := instMfieldOp }
  }

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
    mtypeSpecNext := MsetMType.toMType(mtypeNew)
  }

  io.mtype := mtypeSpec

  // just make verilog more readable
  dontTouch(isMsettypeVec)
}