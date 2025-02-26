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

  private val isMsetMtilem = MSETtilexOpType.isMsetMtilem(func)
  private val isMsetMtilen = MSETtilexOpType.isMsetMtilen(func)
  private val isMsetMtilek = MSETtilexOpType.isMsetMtilek(func)

  private val isMsetMtilexmax = MSETtilexOpType.isMsetMtilexmax(func)

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
    val newmtype : UInt      = UInt(XLEN.W)
    // Function code
    val func  : UInt = FuOpType()
  })

  val out = Output(new Bundle {
    val mtype : MsetMType = MsetMType()
  })
}

class MsetMtypeBaseModule(implicit p: Parameters) extends XSModule {
  val io = IO(new MsetMtypeModuleIO)
}

class MsetMtypeDummyModule(implicit p: Parameters) extends MsetMtypeBaseModule {
  val updatemtype = WireInit(0.U.asTypeOf(io.in.oldmtype))
  updatemtype.msew := "b001".U
  updatemtype.mfp16 := "b01".U

  io.out.mtype := updatemtype
}

class MsetMtypeModule(implicit p: Parameters) extends MsetMtypeBaseModule {
  val updatemtype = WireInit(io.in.oldmtype)
  switch(io.in.func) {
    is (MSETtypeOpType.msetsew)    { updatemtype.msew   := io.in.newmtype(2, 0) }
    is (MSETtypeOpType.msetint4)   { updatemtype.mint4  := io.in.newmtype(0) }
    is (MSETtypeOpType.msetint8)   { updatemtype.mint8  := io.in.newmtype(0) }
    is (MSETtypeOpType.msetint16)  { updatemtype.mint16 := io.in.newmtype(0) }
    is (MSETtypeOpType.msetint32)  { updatemtype.mint32 := io.in.newmtype(0) }
    is (MSETtypeOpType.msetint64)  { updatemtype.mint64 := io.in.newmtype(0) }
    is (MSETtypeOpType.msetfp8)    { updatemtype.mfp8   := io.in.newmtype(1, 0) }
    is (MSETtypeOpType.msetfp16)   { updatemtype.mfp16  := io.in.newmtype(1, 0) }
    is (MSETtypeOpType.msetfp32)   { updatemtype.mfp32  := io.in.newmtype(1, 0) }
    is (MSETtypeOpType.msetfp64)   { updatemtype.mfp64  := io.in.newmtype(0) }
    is (MSETtypeOpType.msetba)     { updatemtype.mba    := io.in.newmtype(0) }
    is (MSETtypeOpType.msettype)   { updatemtype        := io.in.newmtype.asTypeOf(updatemtype) }
    is (MSETtypeOpType.msettypei)  { 
      updatemtype.mfp8   := io.in.newmtype(9, 8)
      updatemtype.mint64 := io.in.newmtype(7)
      updatemtype.mint32 := io.in.newmtype(6)
      updatemtype.mint16 := io.in.newmtype(5)
      updatemtype.mint8  := io.in.newmtype(4)
      updatemtype.mint4  := io.in.newmtype(3)
      updatemtype.msew   := io.in.newmtype(2, 0)
    }
    is (MSETtypeOpType.msettypehi) {
      updatemtype.reserved := Cat(io.in.oldmtype.reserved(46, 3), io.in.newmtype(9, 7))
      updatemtype.mba      := io.in.newmtype(6)
      updatemtype.mfp64    := io.in.newmtype(5, 4)
      updatemtype.mfp32    := io.in.newmtype(3, 2)
      updatemtype.mfp16    := io.in.newmtype(1, 0)
    }
  }
  
  private val outMtype = WireInit(0.U.asTypeOf(updatemtype))

  private val msew : UInt = updatemtype.msew

  // Check illegal cases
  private val log2Msew = msew(MSew.width - 1, 0) +& "b011".U
  private val log2MsewMax = log2Up(ELEN).U
  private val sewIllegal = MSew.isReserved(msew) || (log2Msew > log2MsewMax)
  private val reservedIllegal = updatemtype.reserved.orR
  private val illegal = sewIllegal | reservedIllegal | updatemtype.illegal

  outMtype.illegal := illegal
  outMtype.mba     := Mux(illegal, 0.U, updatemtype.mba)
  outMtype.mfp64   := Mux(illegal, 0.U, updatemtype.mfp64)
  outMtype.mfp32   := Mux(illegal, 0.U, updatemtype.mfp32)
  outMtype.mfp16   := Mux(illegal, 0.U, updatemtype.mfp16)
  outMtype.mfp8    := Mux(illegal, 0.U, updatemtype.mfp8)
  outMtype.mint64  := Mux(illegal, 0.U, updatemtype.mint64)
  outMtype.mint32  := Mux(illegal, 0.U, updatemtype.mint32)
  outMtype.mint16  := Mux(illegal, 0.U, updatemtype.mint16)
  outMtype.mint8   := Mux(illegal, 0.U, updatemtype.mint8)
  outMtype.mint4   := Mux(illegal, 0.U, updatemtype.mint4)
  outMtype.msew    := Mux(illegal, 0.U, updatemtype.msew)

  io.out.mtype := outMtype
}