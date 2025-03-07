package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MSETtilexOpType, MSETtypeOpType, UopSplitType, SelImm, MlduOpType, MstuOpType, MarithOpType, MmvefOpType}
import freechips.rocketchip.amba.ahb.AHBParameters.transBits
import xiangshan.MmulOpType
import xiangshan.SrcType
import xiangshan.SrcType

// Set a specific field in mtype
case class MSETINST(fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1: BitPat = SrcType.imm
    val src2: BitPat = SrcType.imm
    XSDecode(src1, src2, SrcType.X, FuType.msetmtypeiwi, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

// Set the whole mtype
case class MSETTYPEINST(mtypei: Boolean, fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1: BitPat = if (mtypei) SrcType.imm else SrcType.xp
    val src2: BitPat = SrcType.imm
    XSDecode(src1, src2, SrcType.X, FuType.msetmtypeiwi, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

// Set mtilem/n/k
case class MSETTXINST(txi: Boolean, fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.MSET) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1: BitPat = if (txi) SrcType.imm else SrcType.xp
    val src2: BitPat = SrcType.imm
    XSDecode(src1, src2, SrcType.X, FuType.msetmtilexiwi, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

case class MLD(fuOp: BitPat, transposed: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mldu
    val src1: BitPat = SrcType.xp
    val src2: BitPat = SrcType.xp
    val src3: BitPat = SrcType.mp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MST(fuOp: BitPat, transposed: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mstu
    val src1: BitPat = SrcType.xp
    val src2: BitPat = SrcType.xp
    val src3: BitPat = SrcType.mp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MMUL(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mmul
    val src1: BitPat = SrcType.xp
    val src2: BitPat = SrcType.xp
    XSDecode(src1, src2, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MMVE(fu: FuType.OHType, fuOp: BitPat, immStride: Boolean = false, regStride: Boolean = false,
  fWen: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1: BitPat = SrcType.xp
    val src2: BitPat = SrcType.xp
    XSDecode(src1, src2, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = fWen, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MBC(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.marith
    val src1: BitPat = SrcType.xp
    XSDecode(src1, SrcType.X, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MTRANS(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.marith
    val src1: BitPat = SrcType.xp
    XSDecode(src1, SrcType.X, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MARITH(fuOp: BitPat, hasSrc2: Boolean = true) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.marith
    val src1: BitPat = SrcType.mp
    val src2: BitPat = if (hasSrc2) SrcType.mp else SrcType.X
    XSDecode(src1, src2, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MCVT(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.marith
    val src1: BitPat = SrcType.xp
    XSDecode(src1, SrcType.X, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen =F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

object MatrixDecoder extends DecodeConstants {
  val mset: Array[(BitPat, XSDecodeBase)] = Array(
    // MSET contains msetsew, msetint, munsetint, msetfp, munsetfp, msetba
    // It sets specific field in mtype.
    MSETSEW    -> MSETINST(fuOp = MSETtypeOpType.msetsew,   flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETINT4   -> MSETINST(fuOp = MSETtypeOpType.msetint4,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETINT8   -> MSETINST(fuOp = MSETtypeOpType.msetint8,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETINT16  -> MSETINST(fuOp = MSETtypeOpType.msetint16, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETINT32  -> MSETINST(fuOp = MSETtypeOpType.msetint32, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETINT64  -> MSETINST(fuOp = MSETtypeOpType.msetint64, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETFP8    -> MSETINST(fuOp = MSETtypeOpType.msetfp8,   flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETFP16   -> MSETINST(fuOp = MSETtypeOpType.msetfp16,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETFP32   -> MSETINST(fuOp = MSETtypeOpType.msetfp32,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETFP64   -> MSETINST(fuOp = MSETtypeOpType.msetfp64,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    MSETBA     -> MSETINST(fuOp = MSETtypeOpType.msetba,    flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETFIELD),
    
    MSETTYPE   -> MSETTYPEINST(mtypei = F, fuOp = MSETtypeOpType.msettype,   flushPipe = T, blockBack = F, selImm = SelImm.X),
    MSETTYPEHI -> MSETTYPEINST(mtypei = T, fuOp = MSETtypeOpType.msettypehi, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTYPEI  -> MSETTYPEINST(mtypei = T, fuOp = MSETtypeOpType.msettypei,  flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    
    // Set tilem/n/k
    MSETTILEM  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilem_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEMI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilem_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEN  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilen_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILENI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilen_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEK  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilek_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEKI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilek_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    
    // TODO: Sparse config
    // MSETTSP    -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETTSPI   -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETSPI),
    // MSETDSP    -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETDSPI   -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETSPI),
    
    // TODO: Img2col config
    // MSETOUTSH  -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETINSH   -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETSK     -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETPADVAL -> MSETINST(fuOp = MSETtilexOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
  )

  val mls: Array[(BitPat, XSDecodeBase)] = Array(
    // Load left matrix, A
    MLAE8_M -> MLD(MlduOpType.mlae),
    MLAE16_M -> MLD(MlduOpType.mlae),
    MLAE32_M -> MLD(MlduOpType.mlae),
    MLAE64_M -> MLD(MlduOpType.mlae),
    // Load right matrix, B
    MLBE8_M -> MLD(MlduOpType.mlbe),
    MLBE16_M -> MLD(MlduOpType.mlbe),
    MLBE32_M -> MLD(MlduOpType.mlbe),
    MLBE64_M -> MLD(MlduOpType.mlbe),
    // Load output matrix, C
    MLCE8_M -> MLD(MlduOpType.mlce),
    MLCE16_M -> MLD(MlduOpType.mlce),
    MLCE32_M -> MLD(MlduOpType.mlce),
    MLCE64_M -> MLD(MlduOpType.mlce),
    // Load a whole tile matrix from memory without considering the size
    MLTRE8_M -> MLD(MlduOpType.mltre),
    MLTRE16_M -> MLD(MlduOpType.mltre),
    MLTRE32_M -> MLD(MlduOpType.mltre),
    MLTRE64_M -> MLD(MlduOpType.mltre),
    // Load transposed left matrix, A
    MLATE8_M -> MLD(MlduOpType.mlate, transposed = true),
    MLATE16_M -> MLD(MlduOpType.mlate, transposed = true),
    MLATE32_M -> MLD(MlduOpType.mlate, transposed = true),
    MLATE64_M -> MLD(MlduOpType.mlate, transposed = true),
    // Load transposed right matrix, B
    MLBTE8_M -> MLD(MlduOpType.mlbte, transposed = true),
    MLBTE16_M -> MLD(MlduOpType.mlbte, transposed = true),
    MLBTE32_M -> MLD(MlduOpType.mlbte, transposed = true),
    MLBTE64_M -> MLD(MlduOpType.mlbte, transposed = true),
    // Load transposed output matrix, C
    MLCTE8_M -> MLD(MlduOpType.mlcte, transposed = true),
    MLCTE16_M -> MLD(MlduOpType.mlcte, transposed = true),
    MLCTE32_M -> MLD(MlduOpType.mlcte, transposed = true),
    MLCTE64_M -> MLD(MlduOpType.mlcte, transposed = true),
    // Load a whole accumulation matrix from memory without considering the size
    MLACCE8_M -> MLD(MlduOpType.mlacce),
    MLACCE16_M -> MLD(MlduOpType.mlacce),
    MLACCE32_M -> MLD(MlduOpType.mlacce),
    MLACCE64_M -> MLD(MlduOpType.mlacce),
    // Store left matrix, A
    MSAE8_M -> MST(MstuOpType.msae),
    MSAE16_M -> MST(MstuOpType.msae),
    MSAE32_M -> MST(MstuOpType.msae),
    MSAE64_M -> MST(MstuOpType.msae),
    // Store right matrix, B
    MSBE8_M -> MST(MstuOpType.msbe),
    MSBE16_M -> MST(MstuOpType.msbe),
    MSBE32_M -> MST(MstuOpType.msbe),
    MSBE64_M -> MST(MstuOpType.msbe),
    // Store output matrix, C
    MSCE8_M -> MST(MstuOpType.msce),
    MSCE16_M -> MST(MstuOpType.msce),
    MSCE32_M -> MST(MstuOpType.msce),
    MSCE64_M -> MST(MstuOpType.msce),
    // Store a whole tile matrix to memory without considering the size
    MSTRE8_M -> MST(MstuOpType.mstre),
    MSTRE16_M -> MST(MstuOpType.mstre),
    MSTRE32_M -> MST(MstuOpType.mstre),
    MSTRE64_M -> MST(MstuOpType.mstre),
    // Store transposed left matrix, A
    MSATE8_M -> MST(MstuOpType.msate, transposed = T),
    MSATE16_M -> MST(MstuOpType.msate, transposed = T),
    MSATE32_M -> MST(MstuOpType.msate, transposed = T),
    MSATE64_M -> MST(MstuOpType.msate, transposed = T),
    // Store transposed right matrix, B
    MSBTE8_M -> MST(MstuOpType.msbte, transposed = T),
    MSBTE16_M -> MST(MstuOpType.msbte, transposed = T),
    MSBTE32_M -> MST(MstuOpType.msbte, transposed = T),
    MSBTE64_M -> MST(MstuOpType.msbte, transposed = T),
    // Store transposed output matrix, C
    MSCTE8_M -> MST(MstuOpType.mscte, transposed = T),
    MSCTE16_M -> MST(MstuOpType.mscte, transposed = T),
    MSCTE32_M -> MST(MstuOpType.mscte, transposed = T),
    MSCTE64_M -> MST(MstuOpType.mscte, transposed = T),
    // Store a whole accumulation matrix to memory without considering the size
    MSACCE8_M -> MST(MstuOpType.msacce),
    MSACCE16_M -> MST(MstuOpType.msacce),
    MSACCE32_M -> MST(MstuOpType.msacce),
    MSACCE64_M -> MST(MstuOpType.msacce),
    // TODO: Zmv: Matrix for Vector operations
    // MLAE8_V -> MLD(),
    // MLAE16_V -> MLD(),
    // MLAE32_V -> MLD(),
    // MLAE64_V -> MLD(),
    // MLBE8_V -> MLD(),
    // MLBE16_V -> MLD(),
    // MLBE32_V -> MLD(),
    // MLBE64_V -> MLD(),
    // MLCE8_V -> MLD(),
    // MLCE16_V -> MLD(),
    // MLCE32_V -> MLD(),
    // MLCE64_V -> MLD(),
    // MSAE8_V -> MST(),
    // MSAE16_V -> MST(),
    // MSAE32_V -> MST(),
    // MSAE64_V -> MST(),
    // MSBE8_V -> MST(),
    // MSBE16_V -> MST(),
    // MSBE32_V -> MST(),
    // MSBE64_V -> MST(),
    // MSCE8_V -> MST(),
    // MSCE16_V -> MST(),
    // MSCE32_V -> MST(),
    // MSCE64_V -> MST(),

    // TODO: Zmi2c: Im2col Extension
    // MLUFAE8_M -> MLD(),
    // MLUFAE16_M -> MLD(),
    // MLUFAE32_M -> MLD(),
    // MLUFAE64_M -> MLD(),
    // MLUFBE8_M -> MLD(),
    // MLUFBE16_M -> MLD(),
    // MLUFBE32_M -> MLD(),
    // MLUFBE64_M -> MLD(),
    // MLUFCE8_M -> MLD(),
    // MLUFCE16_M -> MLD(),
    // MLUFCE32_M -> MLD(),
    // MLUFCE64_M -> MLD(),
    // TODO: Zmc2i: Col2im Extension
    // MSFDAE8_M -> MST(),
    // MSFDAE16_M -> MST(),
    // MSFDAE32_M -> MST(),
    // MSFDAE64_M -> MST(),
    // MSFDBE8_M -> MST(),
    // MSFDBE16_M -> MST(),
    // MSFDBE32_M -> MST(),
    // MSFDBE64_M -> MST(),
    // MSFDCE8_M -> MST(),
    // MSFDCE16_M -> MST(),
    // MSFDCE32_M -> MST(),
    // MSFDCE64_M -> MST(),
  )

  val mmve: Array[(BitPat, XSDecodeBase)] = Array(
    // 4.4.1 Data Move Instructions between Matrix Registers
    // md[i, rs2*(RLEN/EEW) + j] = ms1[i, j]
    // md is accumulation reg and ms1 is tile reg
    MMVE8_A_T  -> MMVE(FuType.marith, MarithOpType.mmove8AT, regStride = T),
    MMVE16_A_T -> MMVE(FuType.marith, MarithOpType.mmove16AT, regStride = T),
    MMVE32_A_T -> MMVE(FuType.marith, MarithOpType.mmove32AT, regStride = T),
    MMVE64_A_T -> MMVE(FuType.marith, MarithOpType.mmove64AT, regStride = T),
    
    // md[i, j] = md[i, rs2*(RLEN/EEW) + j]
    // md is tile reg and ms1 is accumulation reg
    MMVE8_T_A  -> MMVE(FuType.marith, MarithOpType.mmove8TA, regStride = T),
    MMVE16_T_A -> MMVE(FuType.marith, MarithOpType.mmove16TA, regStride = T),
    MMVE32_T_A -> MMVE(FuType.marith, MarithOpType.mmove32TA, regStride = T),
    MMVE64_T_A -> MMVE(FuType.marith, MarithOpType.mmove64TA, regStride = T),
    
    // md[i, imm * (RLEN / EEW) + j] = ms1[i, j]
    // md is an accumulation register and ms1 is a tile register.
    MMVIE8_A_T  -> MMVE(FuType.marith, MarithOpType.mmove8AT, immStride = T),
    MMVIE16_A_T -> MMVE(FuType.marith, MarithOpType.mmove16AT, immStride = T),
    MMVIE32_A_T -> MMVE(FuType.marith, MarithOpType.mmove32AT, immStride = T),
    MMVIE64_A_T -> MMVE(FuType.marith, MarithOpType.mmove64AT, immStride = T),

    // md[i, j] = ms1[i, imm * (RLEN / EEW) + j]
    // md is a tile register and ms1 is an accumulation register.
    MMVIE8_T_A  -> MMVE(FuType.marith, MarithOpType.mmove8TA, immStride = T),
    MMVIE16_T_A -> MMVE(FuType.marith, MarithOpType.mmove16TA, immStride = T),
    MMVIE32_T_A -> MMVE(FuType.marith, MarithOpType.mmove32TA, immStride = T),
    MMVIE64_T_A -> MMVE(FuType.marith, MarithOpType.mmove64TA, immStride = T),

    // md = ms1, md and ms1 are both accumulation registers.
    MMVE8_A_A  -> MMVE(FuType.marith, MarithOpType.mmove8AA),
    MMVE16_A_A -> MMVE(FuType.marith, MarithOpType.mmove16AA),
    MMVE32_A_A -> MMVE(FuType.marith, MarithOpType.mmove32AA),
    MMVE64_A_A -> MMVE(FuType.marith, MarithOpType.mmove64AA),

    // md = ms1, md and ms1 are both tile registers.
    MMVE8_T_T  -> MMVE(FuType.marith, MarithOpType.mmove8TT),
    MMVE16_T_T -> MMVE(FuType.marith, MarithOpType.mmove16TT),
    MMVE32_T_T -> MMVE(FuType.marith, MarithOpType.mmove32TT),
    MMVE64_T_T -> MMVE(FuType.marith, MarithOpType.mmove64TT),

    // 4.4.2 Data Move Instructions between Matrix and Integer
    // x[rd] = ms1[i, j], i = rs2[15:0], j = rs2[XLEN-1:16]
    // rd is an integer register and ms1 is a tile register.
    // MMVE8_X_T -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE16_X_T -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE32_X_T -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE64_X_T -> MMVE(FuType.mmvei, MarithOpType.placeholder),

    // md[i, j] = x[rs1], i = rs2[15:0], j = rs2[XLEN-1:16]
    // md is a tile register and rs1 is an integer register.
    // MMVE8_T_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE16_T_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE32_T_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE64_T_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),

    // x[rd] = ms1[i, j], i = rs2[15:0], j = rs2[XLEN-1:16]
    // rd is an integer register and ms1 is an accumulation register.
    // MMVE8_X_A -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE16_X_A -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE32_X_A -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE64_X_A -> MMVE(FuType.mmvei, MarithOpType.placeholder),

    // md[i, j] = x[rs1], i = rs2[15:0], j = rs2[XLEN-1:16]
    // md is an accumulation register and rs1 is an integer register.
    // MMVE8_A_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE16_A_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE32_A_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),
    // MMVE64_A_X -> MMVE(FuType.mmvei, MarithOpType.placeholder),

    // 4.4.3 Data Move Instructions between Matrix and Float-point
    // f[rd] = ms1[i, j], i = rs2[15:0], j = rs2[XLEN-1:16]
    // rd is a float-point register and ms1 is a tile register.
    MFMVE8_F_T -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE16_F_T -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE32_F_T -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE64_F_T -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),

    // md[i, j] = f[rs1], i = rs2[15:0], j = rs2[XLEN-1:16]
    // md is a tile register and rs1 is a float-point register.
    MFMVE8_T_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE16_T_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE32_T_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE64_T_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),

    // f[rd] = ms1[i, j], i = rs2[15:0], j = rs2[XLEN-1:16]
    // rd is a float-point register and ms1 is an accumulation register.
    MFMVE8_F_A -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE16_F_A -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE32_F_A -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),
    MFMVE64_F_A -> MMVE(FuType.mmvef, MmvefOpType.placeholder, fWen = T),

    // md[i, j] = f[rs1], i = rs2[15:0], j = rs2[XLEN-1:16]
    // md is an accumulation register and rs1 is a float-point register.
    MFMVE8_A_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE16_A_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE32_A_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),
    MFMVE64_A_F -> MMVE(FuType.mmvef, MmvefOpType.placeholder),

    // 4.4.4 Data Broadcast Instructions
    // Broadcast the first row of a tile register to fill the whole matrix.
    MBCAR_M -> MBC(MarithOpType.mbcARow),
    MBCBR_M -> MBC(MarithOpType.mbcBRow),
    
    // Broadcast the first row of an accumulation register to fill the whole matrix.
    MBCCR_M -> MBC(MarithOpType.mbcCRow),

    // Broadcast the first column of a tile register to fill the whole matrix.
    MBCACE8_M  -> MBC(MarithOpType.mbcACol8),
    MBCACE16_M -> MBC(MarithOpType.mbcACol16),
    MBCACE32_M -> MBC(MarithOpType.mbcACol32),
    MBCACE64_M -> MBC(MarithOpType.mbcACol64),
    MBCBCE8_M  -> MBC(MarithOpType.mbcBCol8),
    MBCBCE16_M -> MBC(MarithOpType.mbcBCol16),
    MBCBCE32_M -> MBC(MarithOpType.mbcBCol32),
    MBCBCE64_M -> MBC(MarithOpType.mbcBCol64),

    // Broadcast the first column of an accumulation register to fill the whole matrix.
    MBCCCE8_M  -> MBC(MarithOpType.mbcCCol8),
    MBCCCE16_M -> MBC(MarithOpType.mbcCCol16),
    MBCCCE32_M -> MBC(MarithOpType.mbcCCol32),
    MBCCCE64_M -> MBC(MarithOpType.mbcCCol64),

    // Broadcast the first element of a tile register to fill the whole matrix.
    MBCAEE8_M  -> MBC(MarithOpType.mbcAEle8),
    MBCAEE16_M -> MBC(MarithOpType.mbcAEle16),
    MBCAEE32_M -> MBC(MarithOpType.mbcAEle32),
    MBCAEE64_M -> MBC(MarithOpType.mbcAEle64),
    MBCBEE8_M  -> MBC(MarithOpType.mbcBEle8),
    MBCBEE16_M -> MBC(MarithOpType.mbcBEle16),
    MBCBEE32_M -> MBC(MarithOpType.mbcBEle32),
    MBCBEE64_M -> MBC(MarithOpType.mbcBEle64),

    // Broadcast the first element of an accumulation register to fill the whole matrix.
    MBCCEE8_M  -> MBC(MarithOpType.mbcCEle8),
    MBCCEE16_M -> MBC(MarithOpType.mbcCEle16),
    MBCCEE32_M -> MBC(MarithOpType.mbcCEle32),
    MBCCEE64_M -> MBC(MarithOpType.mbcCEle64),
    
    // 4.4.5. Matrix Transpose Instructions
    // Transpose square matrix of tile register.
    MTAE8_M  -> MTRANS(MarithOpType.mtransA8),
    MTAE16_M -> MTRANS(MarithOpType.mtransA16),
    MTAE32_M -> MTRANS(MarithOpType.mtransA32),
    MTAE64_M -> MTRANS(MarithOpType.mtransA64),
    MTBE8_M  -> MTRANS(MarithOpType.mtransB8),
    MTBE16_M -> MTRANS(MarithOpType.mtransB16),
    MTBE32_M -> MTRANS(MarithOpType.mtransB32),
    MTBE64_M -> MTRANS(MarithOpType.mtransB64),

    // Transpose square matrix of accumulation register.
    MTCE8_M  -> MTRANS(MarithOpType.mtransC8),
    MTCE16_M -> MTRANS(MarithOpType.mtransC16),
    MTCE32_M -> MTRANS(MarithOpType.mtransC32),
    MTCE64_M -> MTRANS(MarithOpType.mtransC64),
  )

  val mmul: Array[(BitPat, XSDecodeBase)] = Array(
    // 4.5.1. Matrix Multiplication Instructions
    // Unigned integer matrix multiplication and add, md = md + ms1 * ms2.
    // MMAU_MM -> MMUL(MmulOpType.placeholder),
    // MMAU_H_MM -> MMUL(MmulOpType.placeholder),
    // MMAU_W_MM -> MMUL(MmulOpType.placeholder),
    // MMAU_DW_MM -> MMUL(MmulOpType.placeholder),
    // MWMAU_MM -> MMUL(MmulOpType.placeholder),
    // MWMAU_H_MM -> MMUL(MmulOpType.placeholder),
    // MWMAU_W_MM -> MMUL(MmulOpType.placeholder),
    // MQMAU_MM -> MMUL(MmulOpType.placeholder),
    // MQMAU_B_MM -> MMUL(MmulOpType.placeholder),
    // MOMAU_MM -> MMUL(MmulOpType.placeholder),
    // MOMAU_HB_MM -> MMUL(MmulOpType.placeholder),

    // MSMAU_MM -> MMUL(MmulOpType.placeholder),
    // MSMAU_H_MM -> MMUL(MmulOpType.placeholder),
    // MSMAU_W_MM -> MMUL(MmulOpType.placeholder),
    // MSMAU_DW_MM -> MMUL(MmulOpType.placeholder),
    // MSWMAU_MM -> MMUL(MmulOpType.placeholder),
    // MSWMAU_H_MM -> MMUL(MmulOpType.placeholder),
    // MSWMAU_W_MM -> MMUL(MmulOpType.placeholder),
    // MSQMAU_MM -> MMUL(MmulOpType.placeholder),
    // MSQMAU_B_MM -> MMUL(MmulOpType.placeholder),
    // MSOMAU_MM -> MMUL(MmulOpType.placeholder),
    // MSOMAU_HB_MM -> MMUL(MmulOpType.placeholder),

    // Signed integer matrix multiplication and add, md = md + ms1 * ms2.
    // MMA_MM -> MMUL(MmulOpType.placeholder),
    // MMA_H_MM -> MMUL(MmulOpType.placeholder),
    // MMA_W_MM -> MMUL(MmulOpType.placeholder),
    // MMA_DW_MM -> MMUL(MmulOpType.placeholder),
    // MWMA_MM -> MMUL(MmulOpType.placeholder),
    // MWMA_H_MM -> MMUL(MmulOpType.placeholder),
    // MWMA_W_MM -> MMUL(MmulOpType.placeholder),
    // MQMA_MM -> MMUL(MmulOpType.placeholder),
    // MQMA_B_MM -> MMUL(MmulOpType.placeholder),
    // MOMA_MM -> MMUL(MmulOpType.placeholder),
    // MOMA_HB_MM -> MMUL(MmulOpType.placeholder),
    
    // MSMA_MM -> MMUL(MmulOpType.placeholder),
    // MSMA_H_MM -> MMUL(MmulOpType.placeholder),
    // MSMA_W_MM -> MMUL(MmulOpType.placeholder),
    // MSMA_DW_MM -> MMUL(MmulOpType.placeholder),
    // MSWMA_MM -> MMUL(MmulOpType.placeholder),
    // MSWMA_H_MM -> MMUL(MmulOpType.placeholder),
    // MSWMA_W_MM -> MMUL(MmulOpType.placeholder),
    // MSQMA_MM -> MMUL(MmulOpType.placeholder),
    // MSQMA_B_MM -> MMUL(MmulOpType.placeholder),
    // MSOMA_MM -> MMUL(MmulOpType.placeholder),
    // MSOMA_HB_MM -> MMUL(MmulOpType.placeholder),

    // Float point matrix multiplication and add, md = md + ms1 * ms2.
    MFMA_MM     -> MMUL(MmulOpType.placeholder),
    MFMA_HF_MM  -> MMUL(MmulOpType.placeholder),
    MFMA_F_MM   -> MMUL(MmulOpType.placeholder),
    MFMA_D_MM   -> MMUL(MmulOpType.placeholder),

    MFWMA_MM    -> MMUL(MmulOpType.placeholder),
    MFWMA_CF_MM -> MMUL(MmulOpType.placeholder),
    MFWMA_HF_MM -> MMUL(MmulOpType.placeholder),
    MFWMA_F_MM  -> MMUL(MmulOpType.placeholder),
    MFQMA_MM    -> MMUL(MmulOpType.placeholder),
    MFQMA_CF_MM -> MMUL(MmulOpType.placeholder),
  )

  val msparsemul: Array[(BitPat, XSDecodeBase)] = Array(
    // TODO: Sparse matrix multiplication instructions
  )

  val marith: Array[(BitPat, XSDecodeBase)] = Array(
    // 4.5.2 Element-Wise Instructions
    // TODO: Int matrix element-wise arithmetic instructions

    // Fp matrix element-wise arithmetic instructions
    MFADD_MM    -> MARITH(MarithOpType.mfadd8), // It will be overrided by msew in DecodeUnitComp
    MFADD_CF_MM -> MARITH(MarithOpType.mfadd8),
    MFADD_HF_MM -> MARITH(MarithOpType.mfadd16),
    MFADD_F_MM  -> MARITH(MarithOpType.mfadd32),
    MFADD_D_MM  -> MARITH(MarithOpType.mfadd64),

    MFWADD_MM    -> MARITH(MarithOpType.mfwadd8), // It will be overrided by msew in DecodeUnitComp
    MFWADD_CF_MM -> MARITH(MarithOpType.mfwadd8),
    MFWADD_HF_MM -> MARITH(MarithOpType.mfwadd16),
    MFWADD_F_MM  -> MARITH(MarithOpType.mfwadd32),

    MFSUB_MM    -> MARITH(MarithOpType.mfsub8), // It will be overrided by msew in DecodeUnitComp
    MFSUB_CF_MM -> MARITH(MarithOpType.mfsub8),
    MFSUB_HF_MM -> MARITH(MarithOpType.mfsub16),
    MFSUB_F_MM  -> MARITH(MarithOpType.mfsub32),
    MFSUB_D_MM  -> MARITH(MarithOpType.mfsub64),

    MFWSUB_MM    -> MARITH(MarithOpType.mfwsub8), // It will be overrided by msew in DecodeUnitComp
    MFWSUB_CF_MM -> MARITH(MarithOpType.mfwsub8),
    MFWSUB_HF_MM -> MARITH(MarithOpType.mfwsub16),
    MFWSUB_F_MM  -> MARITH(MarithOpType.mfwsub32),

    MFMIN_MM    -> MARITH(MarithOpType.mfmin8), // It will be overrided by msew in DecodeUnitComp
    MFMIN_CF_MM -> MARITH(MarithOpType.mfmin8),
    MFMIN_HF_MM -> MARITH(MarithOpType.mfmin16),
    MFMIN_F_MM  -> MARITH(MarithOpType.mfmin32),
    MFMIN_D_MM  -> MARITH(MarithOpType.mfmin64),

    MFMAX_MM    -> MARITH(MarithOpType.mfmax8), // It will be overrided by msew in DecodeUnitComp
    MFMAX_CF_MM -> MARITH(MarithOpType.mfmax8),
    MFMAX_HF_MM -> MARITH(MarithOpType.mfmax16),
    MFMAX_F_MM  -> MARITH(MarithOpType.mfmax32),
    MFMAX_D_MM  -> MARITH(MarithOpType.mfmax64),

    MFMUL_MM    -> MARITH(MarithOpType.mfmul8), // It will be overrided by msew in DecodeUnitComp
    MFMUL_CF_MM -> MARITH(MarithOpType.mfmul8),
    MFMUL_HF_MM -> MARITH(MarithOpType.mfmul16),
    MFMUL_F_MM  -> MARITH(MarithOpType.mfmul32),
    MFMUL_D_MM  -> MARITH(MarithOpType.mfmul64),

    MFWMUL_MM    -> MARITH(MarithOpType.mfmul8), // It will be overrided by msew in DecodeUnitComp
    MFWMUL_CF_MM -> MARITH(MarithOpType.mfmul8),
    MFWMUL_HF_MM -> MARITH(MarithOpType.mfmul16),
    MFWMUL_F_MM  -> MARITH(MarithOpType.mfmul32),

    MFDIV_MM    -> MARITH(MarithOpType.mfdiv8), // It will be overrided by msew in DecodeUnitComp
    MFDIV_CF_MM -> MARITH(MarithOpType.mfdiv8),
    MFDIV_HF_MM -> MARITH(MarithOpType.mfdiv16),
    MFDIV_F_MM  -> MARITH(MarithOpType.mfdiv32),
    MFDIV_D_MM  -> MARITH(MarithOpType.mfdiv64),
    
    MFSQRT_MM    -> MARITH(MarithOpType.mfsqrt8), // It will be overrided by msew in DecodeUnitComp
    MFSQRT_CF_MM -> MARITH(MarithOpType.mfsqrt8),
    MFSQRT_HF_MM -> MARITH(MarithOpType.mfsqrt16),
    MFSQRT_F_MM  -> MARITH(MarithOpType.mfsqrt32),
    MFSQRT_D_MM  -> MARITH(MarithOpType.mfsqrt64),
  )

  // 4.6. Type-Convert Instructions
  val mcvt: Array[(BitPat, XSDecodeBase)] = Array(
    // TODO: Convert integer to integer
    // ...

    // Convert float to float
    MFCVT_BF_HF_M  -> MCVT(MarithOpType.mcvtFp16ToBf16), // fp16 to bf16
    MFCVT_HF_BF_M  -> MCVT(MarithOpType.mcvtBf16ToFp16), // bf16 to fp16

    MFWCVT_FW_F_M  -> MCVT(MarithOpType.mcvtDoubleWidth), // single-width float to double-width float
    MFWCVT_HF_CF_M -> MCVT(MarithOpType.mcvtFp8ToFp16), // fp8 to fp16
    MFWCVT_F_HF_M  -> MCVT(MarithOpType.mcvtFp16ToFp32), // fp16 to fp32
    MFWCVT_D_F_M   -> MCVT(MarithOpType.mcvtFp32ToFp64), // fp32 to fp64

    MFNCVT_F_FW_M  -> MCVT(MarithOpType.mcvtHalfWidth), // double-width float to single-width float
    MFNCVT_CF_HF_M -> MCVT(MarithOpType.mcvtFp16ToFp8),  // fp16 to fp8
    MFNCVT_HF_F_M  -> MCVT(MarithOpType.mcvtFp32ToFp16), // fp32 to fp16
    MFNCVT_F_D_M   -> MCVT(MarithOpType.mcvtFp64ToFp32), // fp64 to fp32

    // TODO: Convert integer to float
    // ...

    // TODO: Convert float to integer
    // ...
  )

  override val decodeArray: Array[(BitPat, XSDecodeBase)] = mset ++ mls ++ mmve ++ mmul ++ marith ++ mcvt
}