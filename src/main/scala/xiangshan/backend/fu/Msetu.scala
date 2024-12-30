package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles._

class MsetModuleIO(implicit p: Parameters) extends XSBundle {
  // TODO: use a correct width
  // 'mlWidth' is just a placeholder
  private val mlWidth = p(XSCoreParamsKey).mlWidth

  val in = Input(new Bundle {
    // Application tilem/n/k
    // TODO: It seems only one of them is used at a time. So we could use a single UInt.
    // TODO: Moreover, XLEN could be too large for them. Vector uses such a large width so I keep it for now.
    //       Check whether it's necessary.
    val atm   : UInt = UInt(XLEN.W)
    val atn   : UInt = UInt(XLEN.W)
    val atk   : UInt = UInt(XLEN.W)
    // Matrix data type
    val mtype : MsetMType = MsetMType()
    // Function code
    val func  : UInt = FuOpType()
  })

  val out = Output(new Bundle {
    val mconfig : MConfig = MConfig()
    val tmmax   : UInt = UInt(mlWidth.W)
    val tnmax   : UInt = UInt(mlWidth.W)
    val tkmax   : UInt = UInt(mlWidth.W)
  })

  // test bundle for internal state
  val testOut = Output(new Bundle {
    val log2Tmmax : UInt = UInt(3.W)
    val tmmax     : UInt = UInt(mlWidth.W)
    val log2Tnmax : UInt = UInt(3.W)
    val tnmax     : UInt = UInt(mlWidth.W)
    val log2Tkmax : UInt = UInt(3.W)
    val tkmax     : UInt = UInt(mlWidth.W)
  })
}

class MsetModule(implicit p: Parameters) extends XSModule {
  val io = IO(new MsetModuleIO)

  private val atm   = io.in.atm
  private val atn   = io.in.atn
  private val atk   = io.in.atk
  private val func  = io.in.func
  private val mtype = io.in.mtype

  private val outMConfig = io.out.mconfig
  
  // TODO: use a correct width
  // 'mlWidth' is just a placeholder
  private val mlWidth = p(XSCoreParamsKey).mlWidth

  private val isSetTmmax = MatrixSETOpType.isSetTmmax(func)
  private val isSetTnmax = MatrixSETOpType.isSetTnmax(func)
  private val isSetTkmax = MatrixSETOpType.isSetTkmax(func)
  private val isMsettilemi = MatrixSETOpType.isMsettilemi(func)
  private val isMsettileni = MatrixSETOpType.isMsettileni(func)
  private val isMsettileki = MatrixSETOpType.isMsettileki(func)

  private val msew : UInt = mtype.msew

  private val tilem = WireInit(0.U(XLEN.W))
  private val tilen = WireInit(0.U(XLEN.W))
  private val tilek = WireInit(0.U(XLEN.W))

  private val log2Mlen = log2Up(MLEN)
  private val log2Rlen = log2Up(RLEN)
  private val log2Amul = log2Up(AMUL)
  println(s"[MsetModule] log2Mlen: $log2Mlen")
  println(s"[MsetModule] log2Rlen: $log2Rlen")
  println(s"[MsetModule] log2Amul: $log2Amul")
  println(s"[MsetModule] mlWidth: $mlWidth")

  // use 2 bits vsew to store vsew
  // private val log2Vsew = vsew(VSew.width - 1, 0) +& "b011".U
  // TODO: It's copied from vector's implementation ↑.
  // Check whether this is correct.
  private val log2Msew = msew(MSew.width - 1, 0) +& "b011".U

  private val log2Tmmax: UInt = log2Mlen.U(3.W) - log2Rlen.U(3.W)
  private val log2Tnmax_0: UInt = log2Mlen.U(3.W) - log2Rlen.U(3.W)
  private val log2Tnmax_1: UInt = log2Rlen.U(3.W) - log2Msew
  private val log2Tnmax: UInt = Mux(log2Tnmax_0 > log2Tnmax_1, log2Tnmax_1, log2Tnmax_0)
  private val log2Tkmax: UInt = log2Rlen.U(3.W) - log2Msew
  
  private val tmmax = (1.U(mlWidth.W) << log2Tmmax).asUInt
  private val tnmax = (1.U(mlWidth.W) << log2Tnmax).asUInt
  private val tkmax = (1.U(mlWidth.W) << log2Tkmax).asUInt

  private val normalTM = Mux(atm > tmmax, tmmax, atm)
  private val normalTN = Mux(atn > tnmax, tnmax, atn)
  private val normalTK = Mux(atk > tkmax, tkmax, atk)

  tilem := Mux(isMsettilemi, normalTM, Mux(isSetTmmax, tmmax, normalTM))
  tilen := Mux(isMsettileni, normalTN, Mux(isSetTnmax, tnmax, normalTN))
  tilek := Mux(isMsettileki, normalTK, Mux(isSetTkmax, tkmax, normalTK))

  // TODO: Check illegal cases for matrix extension.
  // private val log2Elen = log2Up(ELEN)
  // private val log2Vlmul = vlmul
  // private val log2VsewMax = Mux(log2Vlmul(2), log2Elen.U + log2Vlmul, log2Elen.U)

  // private val sewIllegal = VSew.isReserved(vsew) || (log2Vsew > log2VsewMax)
  // private val lmulIllegal = VLmul.isReserved(vlmul)
  // private val mtypeIllegal = mtype.reserved.orR

  // private val illegal = lmulIllegal | sewIllegal | vtypeIllegal | vtype.illegal
  // TODO: It's a placeholder.
  private val illegal = false.B

  outMConfig.tilem := Mux(illegal, 0.U, tilem)
  outMConfig.tilen := Mux(illegal, 0.U, tilen)
  outMConfig.tilek := Mux(illegal, 0.U, tilek)

  outMConfig.mtype.illegal := illegal
  outMConfig.mtype.mba     := Mux(illegal, 0.U, mtype.mba)
  outMConfig.mtype.mfp64   := Mux(illegal, 0.U, mtype.mfp64)
  outMConfig.mtype.mfp32   := Mux(illegal, 0.U, mtype.mfp32)
  outMConfig.mtype.mfp16   := Mux(illegal, 0.U, mtype.mfp16)
  outMConfig.mtype.mfp8    := Mux(illegal, 0.U, mtype.mfp8)
  outMConfig.mtype.mint64  := Mux(illegal, 0.U, mtype.mint64)
  outMConfig.mtype.mint32  := Mux(illegal, 0.U, mtype.mint32)
  outMConfig.mtype.mint16  := Mux(illegal, 0.U, mtype.mint16)
  outMConfig.mtype.mint8   := Mux(illegal, 0.U, mtype.mint8)
  outMConfig.mtype.mint4   := Mux(illegal, 0.U, mtype.mint4)
  outMConfig.mtype.msew    := Mux(illegal, 0.U, mtype.msew)

  // TODO: Refer to the V extension's spec and implement the logic for matrix extension.
  // private val log2VlenDivVsew = log2Vlen.U(3.W) - log2Vsew
  // private val vlenDivVsew = 1.U(vlWidth.W) << log2VlenDivVsew
  // io.out.vlmax := Mux(vlmax >= vlenDivVsew, vlmax, vlenDivVsew)
  // TODO: They are placeholders.
  io.out.tmmax := tmmax
  io.out.tnmax := tnmax
  io.out.tkmax := tkmax
  
  // Test bundle for internal state
  io.testOut.tmmax := tmmax
  io.testOut.log2Tmmax := log2Tmmax
  io.testOut.tnmax := tnmax
  io.testOut.log2Tnmax := log2Tnmax
  io.testOut.tkmax := tkmax
  io.testOut.log2Tkmax := log2Tkmax
}