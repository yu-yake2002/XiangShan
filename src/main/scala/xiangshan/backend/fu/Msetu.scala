package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles._

class MsetMtilexModuleIO(implicit p: Parameters) extends XSBundle {
  // TODO: use a correct width
  // 'mlWidth' is just a placeholder
  private val mlWidth = p(XSCoreParamsKey).mlWidth

  val in = Input(new Bundle {
    // Application tilem/n/k
    // TODO: XLEN could be too large for it. Vector uses such a large width so I keep it for now.
    //       Check whether it's necessary.
    val atx   : UInt = UInt(XLEN.W)
    // Matrix data type
    val mtype : MsetMType = MsetMType()
    // Function code
    val func  : UInt = FuOpType()
  })

  val out = Output(new Bundle {
    val mtilex  : UInt = UInt(XLEN.W)
    val txmax   : UInt = UInt(mlWidth.W)
  })
}

class MsetMtilexModule(implicit p: Parameters) extends XSModule {
  val io = IO(new MsetMtilexModuleIO)

  private val atx   = io.in.atx
  private val func  = io.in.func
  private val mtype = io.in.mtype

  private val outMtilex = io.out.mtilex

  private val isMsetMtilem = MatrixSETOpType.isMsetMtilem(func)
  private val isMsetMtilen = MatrixSETOpType.isMsetMtilen(func)
  private val isMsetMtilek = MatrixSETOpType.isMsetMtilek(func)

  private val isMsetMtilexmax = MatrixSETOpType.isMsetMtilexmax(func)

  private val msew : UInt = mtype.msew

  private val mtilex = WireInit(0.U(XLEN.W))

  private val log2Mlen = log2Up(MLEN)
  private val log2Rlen = log2Up(RLEN)
  private val log2Amul = log2Up(AMUL)
  // TODO: use a correct width
  // 'mlWidth' is just a placeholder
  private val mlWidth = p(XSCoreParamsKey).mlWidth
  println(s"[MsetModule] log2Mlen: $log2Mlen")
  println(s"[MsetModule] log2Rlen: $log2Rlen")
  println(s"[MsetModule] log2Amul: $log2Amul")
  println(s"[MsetModule] mlWidth: $mlWidth")

  // Calculate the maximum tilex
  private val log2Msew = msew(MSew.width - 1, 0) +& "b011".U
  private val log2Tmmax: UInt = log2Mlen.U(3.W) - log2Rlen.U(3.W)
  private val log2Tnmax_0: UInt = log2Mlen.U(3.W) - log2Rlen.U(3.W)
  private val log2Tnmax_1: UInt = log2Rlen.U(3.W) - log2Msew
  private val log2Tnmax: UInt = Mux(log2Tnmax_0 > log2Tnmax_1, log2Tnmax_1, log2Tnmax_0)
  private val log2Tkmax: UInt = log2Rlen.U(3.W) - log2Msew
  
  // Select the required tilexmax
  private val tilemMax = (1.U(mlWidth.W) << log2Tmmax).asUInt
  private val tilenMax = (1.U(mlWidth.W) << log2Tnmax).asUInt
  private val tilekMax = (1.U(mlWidth.W) << log2Tkmax).asUInt
  private val tilexMax = MuxCase(0.U, Seq(
    isMsetMtilem -> tilemMax,
    isMsetMtilen -> tilenMax,
    isMsetMtilek -> tilekMax
  ))

  private val normalMtilex = Mux(atx > tilexMax, tilexMax, atx)

  mtilex := Mux(isMsetMtilexmax, tilexMax, normalMtilex)

  // Check illegal cases
  private val log2MsewMax = log2Up(ELEN).U
  private val sewIllegal = MSew.isReserved(msew) || (log2Msew > log2MsewMax)
  private val reservedIllegal = mtype.reserved.orR
  private val illegal = sewIllegal | reservedIllegal | mtype.illegal

  outMtilex := Mux(illegal, 0.U, mtilex)
  io.out.txmax := tilexMax
}

class MsetMtypeModuleIO(implicit p: Parameters) extends XSBundle {
  val in = Input(new Bundle {
    val oldmtype : MsetMType = MsetMType()
    val newmtype : MsetMType = MsetMType()
    val mask : UInt = UInt(XLEN.W)
  })

  val out = Output(new Bundle {
    val mtype : MType = MType()
  })
}

class MsetMtypeModule(implicit p: Parameters) extends XSModule {
  val io = IO(new MsetMtypeModuleIO)

  private val mtype = ((io.in.newmtype.asUInt & io.in.mask) |
                       (io.in.oldmtype.asUInt & ~io.in.mask)).asTypeOf(MsetMType())
  
  private val outMtype = io.out.mtype

  private val msew : UInt = mtype.msew

  // Check illegal cases
  private val log2Msew = msew(MSew.width - 1, 0) +& "b011".U
  private val log2MsewMax = log2Up(ELEN).U
  private val sewIllegal = MSew.isReserved(msew) || (log2Msew > log2MsewMax)
  private val reservedIllegal = mtype.reserved.orR
  private val illegal = sewIllegal | reservedIllegal | mtype.illegal

  outMtype.illegal := illegal
  outMtype.mba     := Mux(illegal, 0.U, mtype.mba)
  outMtype.mfp64   := Mux(illegal, 0.U, mtype.mfp64)
  outMtype.mfp32   := Mux(illegal, 0.U, mtype.mfp32)
  outMtype.mfp16   := Mux(illegal, 0.U, mtype.mfp16)
  outMtype.mfp8    := Mux(illegal, 0.U, mtype.mfp8)
  outMtype.mint64  := Mux(illegal, 0.U, mtype.mint64)
  outMtype.mint32  := Mux(illegal, 0.U, mtype.mint32)
  outMtype.mint16  := Mux(illegal, 0.U, mtype.mint16)
  outMtype.mint8   := Mux(illegal, 0.U, mtype.mint8)
  outMtype.mint4   := Mux(illegal, 0.U, mtype.mint4)
  outMtype.msew    := Mux(illegal, 0.U, mtype.msew)
}