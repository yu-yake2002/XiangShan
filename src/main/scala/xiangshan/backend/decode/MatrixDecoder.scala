package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MatrixSETOpType, UopSplitType, SelImm, MlduType, MstuType}
import freechips.rocketchip.amba.ahb.AHBParameters.transBits

// Set a specific field in mtype
case class MSETINST(fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.X) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1 = SrcType.imm
    val src2 = SrcType.imm
    // TODO: MSETINST could be implemented as MSETTYPEINST with a mask
    XSDecode(src1, src2, SrcType.X, FuType.msettypeiwf, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

// Set the whole mtype
case class MSETTYPEINST(mtypei: Boolean, fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.X) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1 = if (mtypei) SrcType.imm else SrcType.xp
    val src2 = SrcType.imm
    XSDecode(src1, src2, SrcType.X, FuType.msettypeiwf, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

// Set mtilem/n/k
case class MSETTXINST(txi: Boolean, fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.X) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1 = if (txi) SrcType.imm else SrcType.xp
    val src2 = SrcType.imm
    XSDecode(src1, src2, SrcType.X, FuType.msettilexiwf, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

case class MLD(fuOp: BitPat, transposed: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mldu
    val src1 = SrcType.xp
    val src2 = SrcType.xp
    val src3 = SrcType.mp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.IMM_X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MST(fuOp: BitPat, transposed: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mstu
    val src1 = SrcType.xp
    val src2 = SrcType.xp
    val src3 = SrcType.mp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MMUL(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mmul
    val src1 = SrcType.xp
    val src2 = SrcType.xp
    XSDecode(src1, src2, SrcType.X, fu, fuOp, SelImm.X, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

object MatrixDecoder extends DecodeConstants {
  val mset: Array[(BitPat, XSDecodeBase)] = Array(
    // MSET contains msetsew, msetint, munsetint, msetfp, munsetfp, msetba
    // It sets specific field in mtype.
    MSET       -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    
    MSETTYPE   -> MSETTYPEINST(mtypei = F, fuOp = MatrixSETOpType.umsettype_xx, flushPipe = T, blockBack = T, selImm = SelImm.X),
    MSETTYPEHI -> MSETTYPEINST(mtypei = T, fuOp = MatrixSETOpType.umsettypeh_xi, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTYPEI  -> MSETTYPEINST(mtypei = T, fuOp = MatrixSETOpType.umsettypel_xi, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    
    // Set tilem/n/k
    MSETTILEM  -> MSETTXINST(txi = F, fuOp = MatrixSETOpType.umsettilem_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEMI -> MSETTXINST(txi = T, fuOp = MatrixSETOpType.umsettilem_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEN  -> MSETTXINST(txi = F, fuOp = MatrixSETOpType.umsettilen_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILENI -> MSETTXINST(txi = T, fuOp = MatrixSETOpType.umsettilen_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEK  -> MSETTXINST(txi = F, fuOp = MatrixSETOpType.umsettilek_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEKI -> MSETTXINST(txi = T, fuOp = MatrixSETOpType.umsettilek_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    
    // TODO: Sparse config
    // MSETTSP    -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETTSPI   -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETSPI),
    // MSETDSP    -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETDSPI   -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSETSPI),
    
    // TODO: Img2col config
    // MSETOUTSH  -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETINSH   -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETSK     -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
    // MSETPADVAL -> MSETINST(fuOp = MatrixSETOpType.placeholder, flushPipe = F, blockBack = F, selImm = SelImm.X),
  )

  val mls: Array[(BitPat, XSDecodeBase)] = Array(
    // Load left matrix, A
    MLAE8_M -> MLD(MlduType.mlae),
    MLAE16_M -> MLD(MlduType.mlae),
    MLAE32_M -> MLD(MlduType.mlae),
    MLAE64_M -> MLD(MlduType.mlae),
    // Load right matrix, B
    MLBE8_M -> MLD(MlduType.mlbe),
    MLBE16_M -> MLD(MlduType.mlbe),
    MLBE32_M -> MLD(MlduType.mlbe),
    MLBE64_M -> MLD(MlduType.mlbe),
    // Load output matrix, C
    MLCE8_M -> MLD(MlduType.mlce),
    MLCE16_M -> MLD(MlduType.mlce),
    MLCE32_M -> MLD(MlduType.mlce),
    MLCE64_M -> MLD(MlduType.mlce),
    // Load a whole tile matrix from memory without considering the size
    MLTRE8_M -> MLD(MlduType.mltre),
    MLTRE16_M -> MLD(MlduType.mltre),
    MLTRE32_M -> MLD(MlduType.mltre),
    MLTRE64_M -> MLD(MlduType.mltre),
    // Load transposed left matrix, A
    MLATE8_M -> MLD(MlduType.mlate, transposed = true),
    MLATE16_M -> MLD(MlduType.mlate, transposed = true),
    MLATE32_M -> MLD(MlduType.mlate, transposed = true),
    MLATE64_M -> MLD(MlduType.mlate, transposed = true),
    // Load transposed right matrix, B
    MLBTE8_M -> MLD(MlduType.mlbte, transposed = true),
    MLBTE16_M -> MLD(MlduType.mlbte, transposed = true),
    MLBTE32_M -> MLD(MlduType.mlbte, transposed = true),
    MLBTE64_M -> MLD(MlduType.mlbte, transposed = true),
    // Load transposed output matrix, C
    MLCTE8_M -> MLD(MlduType.mlcte, transposed = true),
    MLCTE16_M -> MLD(MlduType.mlcte, transposed = true),
    MLCTE32_M -> MLD(MlduType.mlcte, transposed = true),
    MLCTE64_M -> MLD(MlduType.mlcte, transposed = true),
    // Load a whole accumulation matrix from memory without considering the size
    MLACCE8_M -> MLD(MlduType.mlacce),
    MLACCE16_M -> MLD(MlduType.mlacce),
    MLACCE32_M -> MLD(MlduType.mlacce),
    MLACCE64_M -> MLD(MlduType.mlacce),
    // Store left matrix, A
    MSAE8_M -> MST(MstuType.msae),
    MSAE16_M -> MST(MstuType.msae),
    MSAE32_M -> MST(MstuType.msae),
    MSAE64_M -> MST(MstuType.msae),
    // Store right matrix, B
    MSBE8_M -> MST(MstuType.msbe),
    MSBE16_M -> MST(MstuType.msbe),
    MSBE32_M -> MST(MstuType.msbe),
    MSBE64_M -> MST(MstuType.msbe),
    // Store output matrix, C
    MSCE8_M -> MST(MstuType.msce),
    MSCE16_M -> MST(MstuType.msce),
    MSCE32_M -> MST(MstuType.msce),
    MSCE64_M -> MST(MstuType.msce),
    // Store a whole tile matrix to memory without considering the size
    MSTRE8_M -> MST(MstuType.mstre),
    MSTRE16_M -> MST(MstuType.mstre),
    MSTRE32_M -> MST(MstuType.mstre),
    MSTRE64_M -> MST(MstuType.mstre),
    // Store transposed left matrix, A
    MSATE8_M -> MST(MstuType.msate, transposed = true),
    MSATE16_M -> MST(MstuType.msate, transposed = true),
    MSATE32_M -> MST(MstuType.msate, transposed = true),
    MSATE64_M -> MST(MstuType.msate, transposed = true),
    // Store transposed right matrix, B
    MSBTE8_M -> MST(MstuType.msbte, transposed = true),
    MSBTE16_M -> MST(MstuType.msbte, transposed = true),
    MSBTE32_M -> MST(MstuType.msbte, transposed = true),
    MSBTE64_M -> MST(MstuType.msbte, transposed = true),
    // Store transposed output matrix, C
    MSCTE8_M -> MST(MstuType.mscte, transposed = true),
    MSCTE16_M -> MST(MstuType.mscte, transposed = true),
    MSCTE32_M -> MST(MstuType.mscte, transposed = true),
    MSCTE64_M -> MST(MstuType.mscte, transposed = true),
    // Store a whole accumulation matrix to memory without considering the size
    MSACCE8_M -> MST(MstuType.msacce),
    MSACCE16_M -> MST(MstuType.msacce),
    MSACCE32_M -> MST(MstuType.msacce),
    MSACCE64_M -> MST(MstuType.msacce),
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
  )

  val mmul: Array[(BitPat, XSDecodeBase)] = Array(
    MMAU_MM -> MMUL(MmulType.placeholder),
    MMAU_H_MM -> MMUL(MmulType.placeholder),
    MMAU_W_MM -> MMUL(MmulType.placeholder),
    MMAU_DW_MM -> MMUL(MmulType.placeholder),
    MSMAU_MM -> MMUL(MmulType.placeholder),
    MSMAU_H_MM -> MMUL(MmulType.placeholder),
    MSMAU_W_MM -> MMUL(MmulType.placeholder),
    MSMAU_DW_MM -> MMUL(MmulType.placeholder),
    MMA_MM -> MMUL(MmulType.placeholder),
    MMA_H_MM -> MMUL(MmulType.placeholder),
    MMA_W_MM -> MMUL(MmulType.placeholder),
    MMA_DW_MM -> MMUL(MmulType.placeholder),
    MSMA_MM -> MMUL(MmulType.placeholder),
    MSMA_H_MM -> MMUL(MmulType.placeholder),
    MSMA_W_MM -> MMUL(MmulType.placeholder),
    MSMA_DW_MM -> MMUL(MmulType.placeholder),
    MFMA_MM -> MMUL(MmulType.placeholder),
    MFMA_HF_MM -> MMUL(MmulType.placeholder),
    MFMA_F_MM -> MMUL(MmulType.placeholder),
    MFMA_D_MM -> MMUL(MmulType.placeholder),
    MWMAU_MM -> MMUL(MmulType.placeholder),
    MWMAU_H_MM -> MMUL(MmulType.placeholder),
    MWMAU_W_MM -> MMUL(MmulType.placeholder),
    MSWMAU_MM -> MMUL(MmulType.placeholder),
    MSWMAU_H_MM -> MMUL(MmulType.placeholder),
    MSWMAU_W_MM -> MMUL(MmulType.placeholder),
    MWMA_MM -> MMUL(MmulType.placeholder),
    MWMA_H_MM -> MMUL(MmulType.placeholder),
    MWMA_W_MM -> MMUL(MmulType.placeholder),
    MSWMA_MM -> MMUL(MmulType.placeholder),
    MSWMA_H_MM -> MMUL(MmulType.placeholder),
    MSWMA_W_MM -> MMUL(MmulType.placeholder),
    MFWMA_MM -> MMUL(MmulType.placeholder),
    MFWMA_CF_MM -> MMUL(MmulType.placeholder),
    MFWMA_HF_MM -> MMUL(MmulType.placeholder),
    MFWMA_F_MM -> MMUL(MmulType.placeholder),
    MQMAU_MM -> MMUL(MmulType.placeholder),
    MQMAU_B_MM -> MMUL(MmulType.placeholder),
    MSQMAU_MM -> MMUL(MmulType.placeholder),
    MSQMAU_B_MM -> MMUL(MmulType.placeholder),
    MQMA_MM -> MMUL(MmulType.placeholder),
    MQMA_B_MM -> MMUL(MmulType.placeholder),
    MSQMA_MM -> MMUL(MmulType.placeholder),
    MSQMA_B_MM -> MMUL(MmulType.placeholder),
    MFQMA_MM -> MMUL(MmulType.placeholder),
    MFQMA_CF_MM -> MMUL(MmulType.placeholder),
    MOMAU_MM -> MMUL(MmulType.placeholder),
    MOMAU_HB_MM -> MMUL(MmulType.placeholder),
    MSOMAU_MM -> MMUL(MmulType.placeholder),
    MSOMAU_HB_MM -> MMUL(MmulType.placeholder),
    MOMA_MM -> MMUL(MmulType.placeholder),
    MOMA_HB_MM -> MMUL(MmulType.placeholder),
    MSOMA_MM -> MMUL(MmulType.placeholder),
    MSOMA_HB_MM -> MMUL(MmulType.placeholder),
  )

  val msparsemul: Array[(BitPat, XSDecodeBase)] = Array(
  )

  val marith: Array[(BitPat, XSDecodeBase)] = Array(
  )

  val mlogic: Array[(BitPat, XSDecodeBase)] = Array(
  )

  val mcvt: Array[(BitPat, XSDecodeBase)] = Array(
  )

  override val decodeArray: Array[(BitPat, XSDecodeBase)] = mset ++ mls
}