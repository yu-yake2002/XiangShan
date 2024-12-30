package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.MsetMtilexModule
import xiangshan.backend.fu.matrix.Bundles.{MType, MsetMType}
import xiangshan.backend.decode.isa.bitfield.{InstMType, Riscv32BitInst, XSInstBitFields}

class MTypeGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchMType = Input(Bool())
    val walkMType   = Flipped(Valid(new MType))
    val canUpdateMType = Input(Bool())
    val mtype = Output(new MType)
    val msetmlMType = Input(new MType)
    val commitMType = new Bundle {
      val mtype = Flipped(Valid(new MType))
      val hasMsetmtilex = Input(Bool())
    }
  })
  private val instValidVec = io.insts.map(_.valid)
  private val instFieldVec = io.insts.map(_.bits.asTypeOf(new XSInstBitFields))
  // // Only check vsetvli and vsetivli here.
  // // vsetvl will flush pipe, need not to generate new vtype in decode stage.
  // private val isVsetVec = VecInit(instFieldVec.map(fields =>
  //   (fields.OPCODE === "b1010111".U) && (fields.WIDTH === "b111".U) && (
  //     fields.ALL(31) === "b0".U ||
  //     fields.ALL(31, 30) === "b11".U
  //   )
  // ).zip(instValidVec).map { case (isVset, valid) => valid && isVset})
  
  // TODO: Follow the above comment to implement isMsetVec
  private val isMsetVec = VecInit(instFieldVec.map(field => false.B)) // placeholder

  private val firstMsetOH: Vec[Bool] = VecInit(PriorityEncoderOH(isMsetVec))
  private val firstMsetInstField: XSInstBitFields = PriorityMux(firstMsetOH, instFieldVec)

  private val mtypeArch = RegInit(MType.initMtype())
  private val mtypeSpec = RegInit(MType.initMtype())

  private val mtypeArchNext = WireInit(mtypeArch)
  private val mtypeSpecNext = WireInit(mtypeSpec)

  mtypeArch := mtypeArchNext
  mtypeSpec := mtypeSpecNext

  // private val isMsetmli= (firstVsetInstField.OPCODE === "b1010111".U) && 
  //   (firstVsetInstField.WIDTH === "b111".U) && 
  //   (firstVsetInstField.ALL(31) === "b0".U)
  // private val instVType: InstVType = Mux(isVsetvli, firstVsetInstField.ZIMM_VSETVLI.asTypeOf(new InstVType), 
  //   firstVsetInstField.ZIMM_VSETIVLI.asTypeOf(new InstVType))
  // TODO: Follow the above comment to implement instMType
  private val instMType: InstMType = new InstMType() // placeholder
  private val mtypei: MsetMType = MsetMType.fromInstMType(instMType)

  private val msetMtilexModule = Module(new MsetMtilexModule)
  msetMtilexModule.io.in.atx := 0.U
  msetMtilexModule.io.in.mtype := mtypei
  msetMtilexModule.io.in.func := MatrixSETOpType.placeholder

  // FIXME:
  // private val mtypeNew = msetModule.io.out.outval.asTypeOf(new MType)

  when(io.commitMType.hasMsetmtilex) {
    mtypeArchNext := io.msetmlMType
  }.elsewhen(io.commitMType.mtype.valid) {
    mtypeArchNext := io.commitMType.mtype.bits
  }

  private val inHasMset = isMsetVec.asUInt.orR

  when(io.commitMType.hasMsetmtilex) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    mtypeSpecNext := io.msetmlMType
  }.elsewhen(io.walkMType.valid) {
    mtypeSpecNext := io.walkMType.bits
  }.elsewhen(io.walkToArchMType) {
    mtypeSpecNext := mtypeArch
  }.elsewhen(inHasMset && io.canUpdateMType) {
    mtypeSpecNext := mtypeNew
  }

  io.mtype := mtypeSpec

  // just make verilog more readable
  dontTouch(isMsetVec)
}