package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MatrixSETOpType, UopSplitType, SelImm}

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

  override val decodeArray: Array[(BitPat, XSDecodeBase)] = mset
}