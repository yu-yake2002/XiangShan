/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import chisel3._
import chisel3.util._
import utils.NamedUInt
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.XLen
import xiangshan.ExceptionNO._
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.vector._
import xiangshan.backend.fu.matrix.Bundles._
import xiangshan.backend.issue._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.decode.{Imm, ImmUnion}
import xiangshan.backend.fu.matrix.Bundles.MSew

package object xiangshan {
  object SrcType {
    def imm    = "b00000".U
    def pc     = "b00000".U
    def xp     = "b00001".U
    def fp     = "b00010".U
    def vp     = "b00100".U
    def v0     = "b01000".U
    def mx     = "b10000".U
    def no     = "b00000".U // this src read no reg but cannot be Any value

    // alias
    def reg = this.xp
    def DC  = imm // Don't Care
    def X   = BitPat("b00000")

    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isReg(srcType: UInt) = srcType(0)
    def isXp(srcType: UInt) = srcType(0)
    def isFp(srcType: UInt) = srcType(1)
    def isVp(srcType: UInt) = srcType(2)
    def isV0(srcType: UInt) = srcType(3)
    def isMx(srcType: UInt) = srcType(4)
    def isPcOrImm(srcType: UInt) = isPc(srcType) || isImm(srcType)
    def isNotReg(srcType: UInt): Bool = !srcType.orR
    def isVfp(srcType: UInt) = isVp(srcType) || isFp(srcType)
    def apply() = UInt(5.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)

    def isReady(state: UInt): Bool = state === this.rdy
    def isBusy(state: UInt): Bool = state === this.busy
  }

  def FuOpTypeWidth = 9
  object FuOpType {
    def apply() = UInt(FuOpTypeWidth.W)
    def X     = BitPat("b0_0000_0000")
    def FMVXF = BitPat("b1_1000_0000") //for fmv_x_d & fmv_x_w
  }

  object I2fType {
    // move/cvt ## i64/i32(input) ## f64/f32/f16(output) ## hassign
    def fcvt_h_wu = BitPat("b0_0_00_0")
    def fcvt_h_w  = BitPat("b0_0_00_1")
    def fcvt_h_lu = BitPat("b0_1_00_0")
    def fcvt_h_l  = BitPat("b0_1_00_1")

    def fcvt_s_wu = BitPat("b0_0_01_0")
    def fcvt_s_w  = BitPat("b0_0_01_1")
    def fcvt_s_lu = BitPat("b0_1_01_0")
    def fcvt_s_l  = BitPat("b0_1_01_1")

    def fcvt_d_wu = BitPat("b0_0_10_0")
    def fcvt_d_w  = BitPat("b0_0_10_1")
    def fcvt_d_lu = BitPat("b0_1_10_0")
    def fcvt_d_l  = BitPat("b0_1_10_1")

  }
  object VlduType {
    // bit encoding: | vector or scala (2bit) || mop (2bit) | lumop(5bit) |
    // only unit-stride use lumop
    // mop [1:0]
    // 0 0 : unit-stride
    // 0 1 : indexed-unordered
    // 1 0 : strided
    // 1 1 : indexed-ordered
    // lumop[4:0]
    // 0 0 0 0 0 : unit-stride load
    // 0 1 0 0 0 : unit-stride, whole register load
    // 0 1 0 1 1 : unit-stride, mask load, EEW=8
    // 1 0 0 0 0 : unit-stride fault-only-first
    def vle       = "b01_00_00000".U
    def vlr       = "b01_00_01000".U // whole
    def vlm       = "b01_00_01011".U // mask
    def vleff     = "b01_00_10000".U
    def vluxe     = "b01_01_00000".U // index
    def vlse      = "b01_10_00000".U // strided
    def vloxe     = "b01_11_00000".U // index

    def isWhole  (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01000".U && (fuOpType(8) ^ fuOpType(7))
    def isMasked (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01011".U && (fuOpType(8) ^ fuOpType(7))
    def isStrided(fuOpType: UInt): Bool = fuOpType(6, 5) === "b10".U && (fuOpType(8) ^ fuOpType(7))
    def isIndexed(fuOpType: UInt): Bool = fuOpType(5) && (fuOpType(8) ^ fuOpType(7))
    def isVecLd  (fuOpType: UInt): Bool = fuOpType(8, 7) === "b01".U
    def isFof    (fuOpType: UInt): Bool = isVecLd(fuOpType) && fuOpType(4)
  }

  object VstuType {
    // bit encoding: | padding (2bit) || mop (2bit) | sumop(5bit) |
    // only unit-stride use sumop
    // mop [1:0]
    // 0 0 : unit-stride
    // 0 1 : indexed-unordered
    // 1 0 : strided
    // 1 1 : indexed-ordered
    // sumop[4:0]
    // 0 0 0 0 0 : unit-stride load
    // 0 1 0 0 0 : unit-stride, whole register load
    // 0 1 0 1 1 : unit-stride, mask load, EEW=8
    def vse       = "b10_00_00000".U
    def vsr       = "b10_00_01000".U // whole
    def vsm       = "b10_00_01011".U // mask
    def vsuxe     = "b10_01_00000".U // index
    def vsse      = "b10_10_00000".U // strided
    def vsoxe     = "b10_11_00000".U // index

    def isWhole  (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01000".U && (fuOpType(8) ^ fuOpType(7))
    def isMasked (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01011".U && (fuOpType(8) ^ fuOpType(7))
    def isStrided(fuOpType: UInt): Bool = fuOpType(6, 5) === "b10".U && (fuOpType(8) ^ fuOpType(7))
    def isIndexed(fuOpType: UInt): Bool = fuOpType(5) && (fuOpType(8) ^ fuOpType(7))
    def isVecSt  (fuOpType: UInt): Bool = fuOpType(8, 7) === "b10".U
  }

  object IF2VectorType {
    // use last 2 bits for vsew
    def iDup2Vec   = "b1_00".U
    def fDup2Vec   = "b1_01".U
    def immDup2Vec = "b1_10".U
    def i2Vec      = "b0_00".U
    def f2Vec      = "b0_01".U
    def imm2Vec    = "b0_10".U
    def needDup(bits: UInt): Bool = bits(2)
    def isImm(bits: UInt): Bool = bits(1)
    def isFp(bits: UInt): Bool = bits(0)
    def isFmv(bits: UInt): Bool = bits(0) & !bits(2)
    def FMX_D_X    = "b0_01_11".U
    def FMX_W_X    = "b0_01_10".U
    def FMX_H_X   =  "b0_01_01".U
  }

  object CommitType {
    def NORMAL = "b000".U  // int/fp
    def BRANCH = "b001".U  // branch
    def LOAD   = "b010".U  // load
    def STORE  = "b011".U  // store
    def MLS    = "b100".U  // mls

    def apply() = UInt(3.W)
    def isFused(commitType: UInt): Bool = commitType(2)
    def isLoadStore(commitType: UInt): Bool = !isFused(commitType) && commitType(1)
    def lsInstIsStore(commitType: UInt): Bool = commitType(0)
    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1) && !isFused(commitType)
  }

  object RedirectLevel {
    def flushAfter = "b0".U
    def flush      = "b1".U

    def apply() = UInt(1.W)
    // def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0)
    // def isException(level: UInt) = level(1) && level(0)
  }

  object ExceptionVec {
    val ExceptionVecSize = 24
    def apply() = Vec(ExceptionVecSize, Bool())
    def apply(init: Bool) = VecInit(Seq.fill(ExceptionVecSize)(init))
  }

  object PMAMode {
    def R = "b1".U << 0 //readable
    def W = "b1".U << 1 //writeable
    def X = "b1".U << 2 //executable
    def I = "b1".U << 3 //cacheable: icache
    def D = "b1".U << 4 //cacheable: dcache
    def S = "b1".U << 5 //enable speculative access
    def A = "b1".U << 6 //enable atomic operation, A imply R & W
    def C = "b1".U << 7 //if it is cacheable is configable
    def Reserved = "b0".U

    def apply() = UInt(7.W)

    def read(mode: UInt) = mode(0)
    def write(mode: UInt) = mode(1)
    def execute(mode: UInt) = mode(2)
    def icache(mode: UInt) = mode(3)
    def dcache(mode: UInt) = mode(4)
    def speculate(mode: UInt) = mode(5)
    def atomic(mode: UInt) = mode(6)
    def configable_cache(mode: UInt) = mode(7)

    def strToMode(s: String) = {
      var result = 0.U(8.W)
      if (s.toUpperCase.indexOf("R") >= 0) result = result + R
      if (s.toUpperCase.indexOf("W") >= 0) result = result + W
      if (s.toUpperCase.indexOf("X") >= 0) result = result + X
      if (s.toUpperCase.indexOf("I") >= 0) result = result + I
      if (s.toUpperCase.indexOf("D") >= 0) result = result + D
      if (s.toUpperCase.indexOf("S") >= 0) result = result + S
      if (s.toUpperCase.indexOf("A") >= 0) result = result + A
      if (s.toUpperCase.indexOf("C") >= 0) result = result + C
      result
    }
  }


  object CSROpType {
    //                 | func3|
    def jmp     = "b010_000".U
    def wfi     = "b100_000".U
    def wrs_nto = "b100_010".U
    def wrs_sto = "b100_011".U
    def wrt     = "b001_001".U
    def set     = "b001_010".U
    def clr     = "b001_011".U
    def wrti    = "b001_101".U
    def seti    = "b001_110".U
    def clri    = "b001_111".U

    def isSystemOp (op: UInt): Bool = op(4)
    def isWfi      (op: UInt): Bool = op(5) && !op(1)
    def isWrsNto   (op: UInt): Bool = op(5) && op(1, 0) === "b10".U
    def isWrsSto   (op: UInt): Bool = op(5) && op(1, 0) === "b11".U
    def isCsrAccess(op: UInt): Bool = op(3)
    def isReadOnly (op: UInt): Bool = op(3) && op(2, 0) === 0.U
    def notReadOnly(op: UInt): Bool = op(3) && op(2, 0) =/= 0.U
    def isCSRRW    (op: UInt): Bool = op(3) && op(1, 0) === "b01".U
    def isCSRRSorRC(op: UInt): Bool = op(3) && op(1)

    def getCSROp(op: UInt) = op(1, 0)
    def needImm(op: UInt) = op(2)

    def getFunc3(op: UInt) = op(2, 0)
  }

  // jump
  object JumpOpType {
    def jal  = "b00".U
    def jalr = "b01".U
    def auipc = "b10".U
//    def call = "b11_011".U
//    def ret  = "b11_100".U
    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
  }

  object FenceOpType {
    def fence  = "b10000".U
    def sfence = "b10001".U
    def fencei = "b10010".U
    def hfence_v = "b10011".U
    def hfence_g = "b10100".U
    def nofence= "b00000".U
  }

  object ALUOpType {
    // shift optype
    def slliuw     = "b000_0000".U // slliuw: ZEXT(src1[31:0]) << shamt
    def sll        = "b000_0001".U // sll:     src1 << src2

    def bclr       = "b000_0010".U // bclr:    src1 & ~(1 << src2[5:0])
    def bset       = "b000_0011".U // bset:    src1 | (1 << src2[5:0])
    def binv       = "b000_0100".U // binv:    src1 ^ ~(1 << src2[5:0])

    def srl        = "b000_0101".U // srl:     src1 >> src2
    def bext       = "b000_0110".U // bext:    (src1 >> src2)[0]
    def sra        = "b000_0111".U // sra:     src1 >> src2 (arithmetic)

    def rol        = "b000_1001".U // rol:     (src1 << src2) | (src1 >> (xlen - src2))
    def ror        = "b000_1011".U // ror:     (src1 >> src2) | (src1 << (xlen - src2))

    // RV64 32bit optype
    def addw       = "b001_0000".U // addw:      SEXT((src1 + src2)[31:0])
    def oddaddw    = "b001_0001".U // oddaddw:   SEXT((src1[0] + src2)[31:0])
    def subw       = "b001_0010".U // subw:      SEXT((src1 - src2)[31:0])
    def lui32addw  = "b001_0011".U // lui32addw: SEXT(SEXT(src2[11:0], 32) + {src2[31:12], 12'b0}, 64)

    def addwbit    = "b001_0100".U // addwbit:   (src1 + src2)[0]
    def addwbyte   = "b001_0101".U // addwbyte:  (src1 + src2)[7:0]
    def addwzexth  = "b001_0110".U // addwzexth: ZEXT((src1  + src2)[15:0])
    def addwsexth  = "b001_0111".U // addwsexth: SEXT((src1  + src2)[15:0])

    def sllw       = "b001_1000".U // sllw:     SEXT((src1 << src2)[31:0])
    def srlw       = "b001_1001".U // srlw:     SEXT((src1[31:0] >> src2)[31:0])
    def sraw       = "b001_1010".U // sraw:     SEXT((src1[31:0] >> src2)[31:0])
    def rolw       = "b001_1100".U
    def rorw       = "b001_1101".U

    // ADD-op
    def adduw      = "b010_0000".U // adduw:  src1[31:0]  + src2
    def add        = "b010_0001".U // add:     src1        + src2
    def oddadd     = "b010_0010".U // oddadd:  src1[0]     + src2
    def lui32add   = "b010_0011".U // lui32add: SEXT(src2[11:0]) + {src2[63:12], 12'b0}

    def sr29add    = "b010_0100".U // sr29add: src1[63:29] + src2
    def sr30add    = "b010_0101".U // sr30add: src1[63:30] + src2
    def sr31add    = "b010_0110".U // sr31add: src1[63:31] + src2
    def sr32add    = "b010_0111".U // sr32add: src1[63:32] + src2

    def sh1adduw   = "b010_1000".U // sh1adduw: {src1[31:0], 1'b0} + src2
    def sh1add     = "b010_1001".U // sh1add: {src1[62:0], 1'b0} + src2
    def sh2adduw   = "b010_1010".U // sh2add_uw: {src1[31:0], 2'b0} + src2
    def sh2add     = "b010_1011".U // sh2add: {src1[61:0], 2'b0} + src2
    def sh3adduw   = "b010_1100".U // sh3add_uw: {src1[31:0], 3'b0} + src2
    def sh3add     = "b010_1101".U // sh3add: {src1[60:0], 3'b0} + src2
    def sh4add     = "b010_1111".U // sh4add: {src1[59:0], 4'b0} + src2

    // SUB-op: src1 - src2
    def sub        = "b011_0000".U
    def sltu       = "b011_0001".U
    def slt        = "b011_0010".U
    def maxu       = "b011_0100".U
    def minu       = "b011_0101".U
    def max        = "b011_0110".U
    def min        = "b011_0111".U

    // Zicond
    def czero_eqz  = "b111_0100".U
    def czero_nez  = "b111_0110".U

    // misc optype
    def and        = "b100_0000".U
    def andn       = "b100_0001".U
    def or         = "b100_0010".U
    def orn        = "b100_0011".U
    def xor        = "b100_0100".U
    def xnor       = "b100_0101".U
    def orcb       = "b100_0110".U

    def sextb      = "b100_1000".U
    def packh      = "b100_1001".U
    def sexth      = "b100_1010".U
    def packw      = "b100_1011".U

    def revb       = "b101_0000".U
    def rev8       = "b101_0001".U
    def pack       = "b101_0010".U
    def orh48      = "b101_0011".U

    def szewl1     = "b101_1000".U
    def szewl2     = "b101_1001".U
    def szewl3     = "b101_1010".U
    def byte2      = "b101_1011".U

    def andlsb     = "b110_0000".U
    def andzexth   = "b110_0001".U
    def orlsb      = "b110_0010".U
    def orzexth    = "b110_0011".U
    def xorlsb     = "b110_0100".U
    def xorzexth   = "b110_0101".U
    def orcblsb    = "b110_0110".U
    def orcbzexth  = "b110_0111".U

    def isAddw(func: UInt) = func(6, 4) === "b001".U && !func(3) && !func(1)
    def isSimpleLogic(func: UInt) = func(6, 4) === "b100".U && !func(0)
    def logicToLsb(func: UInt) = Cat("b110".U(3.W), func(3, 1), 0.U(1.W))
    def logicToZexth(func: UInt) = Cat("b110".U(3.W), func(3, 1), 1.U(1.W))

    def apply() = UInt(FuOpTypeWidth.W)
  }

  object VSETOpType {
    val setVlmaxBit = 0
    val keepVlBit   = 1
    // destTypeBit == 0: write vl to rd
    // destTypeBit == 1: write vconfig
    val destTypeBit = 5

    // vsetvli's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1), w(vconfig)     | x[rs1],vtypei  -> vconfig
    //     uop1: r(rs1), w(rd)          | x[rs1],vtypei  -> x[rd]
    def uvsetvcfg_xi        = "b1010_0000".U
    def uvsetrd_xi          = "b1000_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: w(vconfig)             | vlmax, vtypei  -> vconfig
    //     uop1: w(rd)                  | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_i   = "b1010_0001".U
    def uvsetrd_vlmax_i     = "b1000_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(vconfig), w(vconfig) | ld_vconfig.vl, vtypei -> vconfig
    def uvsetvcfg_keep_v    = "b1010_0010".U

    // vsetvl's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1,rs2), w(vconfig) | x[rs1],x[rs2]  -> vconfig
    //     uop1: r(rs1,rs2), w(rd)      | x[rs1],x[rs2]  -> x[rd]
    def uvsetvcfg_xx        = "b0110_0000".U
    def uvsetrd_xx          = "b0100_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: r(rs2), w(vconfig)     | vlmax, vtypei  -> vconfig
    //     uop1: r(rs2), w(rd)          | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_x   = "b0110_0001".U
    def uvsetrd_vlmax_x     = "b0100_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(rs2), w(vtmp)             | x[rs2]               -> vtmp
    //     uop0: r(vconfig,vtmp), w(vconfig) | old_vconfig.vl, vtmp -> vconfig
    def uvmv_v_x            = "b0110_0010".U
    def uvsetvcfg_vv        = "b0111_0010".U

    // vsetivli's uop
    //     uop0: w(vconfig)             | vli, vtypei    -> vconfig
    //     uop1: w(rd)                  | vli, vtypei    -> x[rd]
    def uvsetvcfg_ii        = "b0010_0000".U
    def uvsetrd_ii          = "b0000_0000".U

    // read vec, write int
    // keep vl
    def csrrvl              = "b0001_0110".U

    def isVsetvl  (func: UInt)  = func(6)
    def isVsetvli (func: UInt)  = func(7)
    def isVsetivli(func: UInt)  = func(7, 6) === 0.U
    def isNormal  (func: UInt)  = func(1, 0) === 0.U
    def isSetVlmax(func: UInt)  = func(setVlmaxBit)
    def isKeepVl  (func: UInt)  = func(keepVlBit)
    // RG: region
    def writeIntRG(func: UInt)  = !func(5)
    def writeVecRG(func: UInt)  = func(5)
    def readIntRG (func: UInt)  = !func(4)
    def readVecRG (func: UInt)  = func(4)
    // modify fuOpType
    def keepVl(func: UInt)      = func | (1 << keepVlBit).U
    def setVlmax(func: UInt)    = func | (1 << setVlmaxBit).U
  }

  object MSETtypeOpType {
    def msetsew    = "b0_000_0000".U
    def msetint4   = "b0_000_0001".U
    def msetint8   = "b0_000_0010".U
    def msetint16  = "b0_000_0011".U
    def msetint32  = "b0_000_0100".U
    def msetint64  = "b0_000_0101".U
    def msetfp8    = "b0_000_0110".U
    def msetfp16   = "b0_000_0111".U
    def msetfp32   = "b0_000_1000".U
    def msetfp64   = "b0_000_1001".U
    def msetba     = "b0_000_1010".U

    def msettype   = "b1_001_0000".U
    def msettypei  = "b0_001_0001".U
    def msettypehi = "b0_001_0010".U

    def isMsetTypeFromReg = (func: UInt) => func(7)
    def isMsetTypeFromImm = (func: UInt) => !func(7)
  }

  object MSETtilexOpType {
    def placeholder = "b1100_0000".U

    def isSet  (func: UInt) = func(7) === "b0".U
    def isRead (func: UInt) = func(7) === "b1".U

    def isTileM (func: UInt) = func(3, 0) === "b0000".U
    def isTileN (func: UInt) = func(3, 0) === "b0001".U
    def isTileK (func: UInt) = func(3, 0) === "b0010".U
    def isMRd   (func: UInt) = func(3, 0) === "b0100".U
    
    def isSetX         (func: UInt) = isSet(func) && func(6, 4) === "b000".U
    def isSetImm       (func: UInt) = isSet(func) && func(6, 4) === "b001".U
    def isSetMaxMtilem (func: UInt) = isSet(func) && func(6, 4) === "b100".U
    def isSetMaxMtilen (func: UInt) = isSet(func) && func(6, 4) === "b101".U
    def isSetMaxMtilek (func: UInt) = isSet(func) && func(6, 4) === "b110".U
    def isSetKeep      (func: UInt) = isSet(func) && func(6, 4) === "b111".U

    // msettilex's uop
    //   case1: rs1!=x0, normal
    //     uop0: r(rs1), w(mtilex) | x[rs1] -> mtilex
    def umsettilem_x = "b0_000_0000".U
    def umsettilen_x = "b0_000_0001".U
    def umsettilek_x = "b0_000_0010".U
    //     uop1: r(rs1), w(rd)     | x[rs1] -> x[rd]
    def umsetrd_xx   = "b0_000_0100".U
    //   case2: rs1==x0, rd!=x0, set mtilex to max, set rd to max
    //     uop0: w(mconfig)        | mtilexmax -> mconfig
    def umsetmtilem_mtilemmax = "b0_100_0000".U
    def umsetmtilen_mtilenmax = "b0_101_0001".U
    def umsetmtilek_mtilekmax = "b0_110_0010".U
    //     uop1: w(rd)             | mtilexmax -> x[rd]
    def umsetrd_mtilemmax = "b0_100_0100".U
    def umsetrd_mtilenmax = "b0_101_0100".U
    def umsetrd_mtilekmax = "b0_110_0100".U
    //   case3: rs1==x0, rd==x0, keep mtilex
    def umsettilem_vv = "b0_000_0000".U // TODO: Implement me!

    // msettilexi's uop
    //   case1: rs1!=x0, normal
    //     uop0: w(mtilex)         | imm -> mconfig
    def umsettilem_i = "b0_001_0000".U
    def umsettilen_i = "b0_001_0001".U
    def umsettilek_i = "b0_001_0010".U
    //     uop1: w(rd)             | imm -> x[rd]
    def umsetrd_i    = "b0_001_0100".U
    //   case2: rs1==x0, rd!=x0, set mtilex to max, set rd to max
    //     uop0: w(mconfig)        | mtilexmax -> mconfig
    // def umsetmtilem_mtilemmax = "b0_100_0000".U
    // def umsetmtilen_mtilenmax = "b0_101_0001".U
    // def umsetmtilek_mtilekmax = "b0_110_0010".U
    //     uop1: w(rd)             | mtilexmax -> x[rd]
    // def umsetrd_mtilemmax    = "b0_100_0100".U
    // def umsetrd_mtilenmax    = "b0_101_0100".U
    // def umsetrd_mtilekmax    = "b0_110_0100".U
    //   case3: rs1==x0, rd==x0, keep mtilex
    //     uop0: r(mconfig), w(mconfig) | ld_mconfig.mtilex, imm -> mconfig
    def umsettilem_keep_i = "b0_000_0000".U // TODO: Implement me!

    // read mtilex
    def csrrmtilem    = "b1_000_0000".U
    def csrrmtilen    = "b1_000_0001".U
    def csrrmtilek    = "b1_000_0010".U
    
    def isMsettilem (func: UInt)  = isSetX(func)         && isTileM(func)
    def isMsettilemi (func: UInt) = isSetImm(func)       && isTileM(func)
    def isMsetTilemMax (func: UInt)   = isSetMaxMtilem(func) && isTileM(func)
    def isMsettilen (func: UInt)  = isSetX(func)         && isTileN(func)
    def isMsettileni (func: UInt) = isSetImm(func)       && isTileN(func)
    def isMsetTilenMax (func: UInt)   = isSetMaxMtilen(func) && isTileN(func)
    def isMsettilek (func: UInt)  = isSetX(func)         && isTileK(func)
    def isMsettileki (func: UInt) = isSetImm(func)       && isTileK(func)
    def isMsetTilekMax (func: UInt)   = isSetMaxMtilek(func) && isTileK(func)

    def isMsettilexi (func: UInt) = isSetImm(func) && (isTileM(func) || isTileN(func) || isTileK(func))
    def isMsettilex (func: UInt)  = isSetX(func) && (isTileM(func) || isTileN(func) || isTileK(func))
    def isMsetMtilexmax (func: UInt) = isSet(func) && (isSetMaxMtilem(func) || isSetMaxMtilen(func) || isSetMaxMtilek(func))

    def isMsetMtilem (func: UInt)  = isSet(func)  && isTileM(func)
    def isMsetMtilen (func: UInt)  = isSet(func)  && isTileN(func)
    def isMsetMtilek (func: UInt)  = isSet(func)  && isTileK(func)
    def isMsetMtilex (func: UInt)  = isSet(func)  && (isTileM(func) || isTileN(func) || isTileK(func))
    def isMreadMtilem (func: UInt) = isRead(func) && isTileM(func)
    def isMreadMtilen (func: UInt) = isRead(func) && isTileN(func)
    def isMreadMtilek (func: UInt) = isRead(func) && isTileK(func)
    def isMreadMtilex (func: UInt) = isRead(func) && (isTileM(func) || isTileN(func) || isTileK(func))

    // def isMsetMtypeFromImm (func: UInt) = isSet(func) && (isSetImm(func) || isSetImmH(func) || isSetImmL(func)) && isMType(func)

    def toMxIdx (func: UInt) = func(1, 0)
  }

  object MldstOpType {
    def placeholder = "b0_00000_0_00".U

    // bit encoding: ldst (1b) | matrix type (5b) | transposed (1b) | width (2b)
    // matrix type [4:0]
    // 0 0 0 0 1 : output matrix, C
    // 0 0 0 1 0 : left matrix, A
    // 0 0 1 0 0 : right matrix, B
    // 0 1 0 0 0 : tile matrix without considering the size
    // 1 0 0 0 0 : accumulation matrix without considering the size
    def isLoad (func: UInt) = func(8) === "b0".U
    def isStore(func: UInt) = func(8) === "b1".U

    def isMatrixC (func: UInt) = func(7, 3) === "b00001".U
    def isMatrixA (func: UInt) = func(7, 3) === "b00010".U
    def isMatrixB (func: UInt) = func(7, 3) === "b00100".U
    def isTile    (func: UInt) = func(7, 3) === "b01000".U
    def isAccum   (func: UInt) = func(7, 3) === "b10000".U

    def isUntransposed (func: UInt) = func(2) === "b0".U
    def isTransposed   (func: UInt) = func(2) === "b1".U
    
    def isFp8  (func: UInt) = func(1, 0) === "b00".U
    def isFp16 (func: UInt) = func(1, 0) === "b01".U
    def isFp32 (func: UInt) = func(1, 0) === "b10".U
    // def isFp64 (func: UInt) = func(1, 0) === "b11".U
    
    def mlaeFp8    = "b0_00001_0_00".U
    def mlaeFp16   = "b0_00001_0_01".U
    def mlaeFp32   = "b0_00001_0_10".U
    def mlateFp8   = "b0_00001_1_00".U
    def mlateFp16  = "b0_00001_1_01".U
    def mlateFp32  = "b0_00001_1_10".U
    def mlbeFp8    = "b0_00010_0_00".U
    def mlbeFp16   = "b0_00010_0_01".U
    def mlbeFp32   = "b0_00010_0_10".U
    def mlbteFp8   = "b0_00010_1_00".U
    def mlbteFp16  = "b0_00010_1_01".U
    def mlbteFp32  = "b0_00010_1_10".U
    def mlceFp8    = "b0_00100_0_00".U
    def mlceFp16   = "b0_00100_0_01".U
    def mlceFp32   = "b0_00100_0_10".U
    def mlcteFp8   = "b0_00100_1_00".U
    def mlcteFp16  = "b0_00100_1_01".U
    def mlcteFp32  = "b0_00100_1_10".U

    def mltreFp8   = "b0_01000_0_00".U
    def mltreFp16  = "b0_01000_0_01".U
    def mltreFp32  = "b0_01000_0_10".U
    def mlacceFp8  = "b0_10000_0_00".U
    def mlacceFp16 = "b0_10000_0_01".U
    def mlacceFp32 = "b0_10000_0_10".U

    def msaeFp8    = "b1_00001_0_00".U
    def msaeFp16   = "b1_00001_0_01".U
    def msaeFp32   = "b1_00001_0_10".U
    def msateFp8   = "b1_00001_1_00".U
    def msateFp16  = "b1_00001_1_01".U
    def msateFp32  = "b1_00001_1_10".U
    def msbeFp8    = "b1_00010_0_00".U
    def msbeFp16   = "b1_00010_0_01".U
    def msbeFp32   = "b1_00010_0_10".U
    def msbteFp8   = "b1_00010_1_00".U
    def msbteFp16  = "b1_00010_1_01".U
    def msbteFp32  = "b1_00010_1_10".U
    def msceFp8    = "b1_00100_0_00".U
    def msceFp16   = "b1_00100_0_01".U
    def msceFp32   = "b1_00100_0_10".U
    def mscteFp8   = "b1_00100_1_00".U
    def mscteFp16  = "b1_00100_1_01".U
    def mscteFp32  = "b1_00100_1_10".U

    def mstreFp8   = "b1_01000_0_00".U
    def mstreFp16  = "b1_01000_0_01".U
    def mstreFp32  = "b1_01000_0_10".U
    def msacceFp8  = "b1_10000_0_00".U
    def msacceFp16 = "b1_10000_0_01".U
    def msacceFp32 = "b1_10000_0_10".U
  }

  object MmulOpType {
    def placeholder = "b11_111_111".U

    def isInt         (func: UInt) = func(7) === "b0".U
    def isFloat       (func: UInt) = func(7) === "b1".U

    def isSigned      (func: UInt) = func(6) === "b1".U // only for int
    def isUnsigned    (func: UInt) = func(6) === "b0".U // only for int

    def isFromE4   (func: UInt) = func(5, 3) === MSew.e4
    def isFromE8   (func: UInt) = func(5, 3) === MSew.e8
    def isFromE16  (func: UInt) = func(5, 3) === MSew.e16
    def isFromE32  (func: UInt) = func(5, 3) === MSew.e32
    def isFromE64  (func: UInt) = func(5, 3) === MSew.e64
    def isFromMsew (func: UInt) = func(5, 3) === "b100".U

    def isTo1W     (func: UInt) = func(2, 1) === "b00".U
    def isTo2W     (func: UInt) = func(2, 1) === "b01".U
    def isTo4W     (func: UInt) = func(2, 1) === "b10".U

    def isSat      (func: UInt) = func(0) === "b1".U

    def hfmaFp8ToFp8   = "b10_000_00_0".U
    def hfmaFp8ToFp16  = "b10_000_01_0".U
    def hfmaFp8ToFp32  = "b10_000_10_0".U

    def hfmaFp16ToFp16 = "b10_001_00_0".U
    def hfmaFp16ToFp32 = "b10_001_01_0".U

    def hfmaFp32ToFp32 = "b10_010_00_0".U

    def hfmaFpxToFpx   = "b10_100_00_0".U
    def hfmaFpxToFp2x  = "b10_100_01_0".U

    def getFromType (func: UInt): UInt = func(5, 3)
    def getToType   (func: UInt): UInt = {
      val wide = WireInit(func(2, 1))
      val toType = WireInit(0.U(3.W))
      switch(wide) {
        is("b00".U) {
          toType := getFromType(func)
        }
        is("b01".U) {
          toType := MuxLookup(getFromType(func), "b100".U)(Seq(
            MSew.e4  -> MSew.e8,
            MSew.e8  -> MSew.e16,
            MSew.e16 -> MSew.e32,
            MSew.e32 -> MSew.e64
          ))
        }
        is("b10".U) {
          toType := MuxLookup(getFromType(func), "b100".U)(Seq(
            MSew.e4  -> MSew.e16,
            MSew.e8  -> MSew.e32,
            MSew.e16 -> MSew.e64
          ))
        }
        is("b11".U) {
          toType := "b100".U
        }
      }
      toType
    }
  }

  object MarithOpType {
    def placeholder = "b111_111_111".U

    // 4.6 Type Convert
    def isCvt      (func: UInt) = func(8, 6) === "b000".U

    def isFromE4M3 (func: UInt) = func(5, 3) === "b000".U
    def isFromE5M2 (func: UInt) = func(5, 3) === "b001".U
    def isFromE3M4 (func: UInt) = func(5, 3) === "b010".U
    def isFromFp16 (func: UInt) = func(5, 3) === "b011".U // E5M10
    def isFromBf16 (func: UInt) = func(5, 3) === "b100".U // E8M7
    def isFromFp32 (func: UInt) = func(5, 3) === "b101".U // E8M23
    def isFromTf32 (func: UInt) = func(5, 3) === "b110".U // E8M10
    def isFromFp64 (func: UInt) = func(5, 3) === "b111".U

    def isToE4M3 (func: UInt) = func(2, 0) === "b000".U
    def isToE5M2 (func: UInt) = func(2, 0) === "b001".U
    def isToE3M4 (func: UInt) = func(2, 0) === "b010".U
    def isToFp16 (func: UInt) = func(2, 0) === "b011".U // E5M10
    def isToBf16 (func: UInt) = func(2, 0) === "b100".U // E8M7
    def isToFp32 (func: UInt) = func(2, 0) === "b101".U // E8M23
    def isToTf32 (func: UInt) = func(2, 0) === "b110".U // E8M10
    def isToFp64 (func: UInt) = func(2, 0) === "b111".U
    
    // TODO: Support more type convert

    def mcvtFp8ToFp8   = "b000_000_000".U
    def mcvtFp8ToFp16  = "b000_000_011".U
    def mcvtFp16ToFp8  = "b000_011_000".U
    def mcvtFp16ToBf16 = "b000_011_100".U
    def mcvtFp16ToFp32 = "b000_011_101".U
    def mcvtBf16ToFp16 = "b000_100_011".U
    def mcvtFp32ToFp16 = "b000_101_011".U
    def mcvtFp32ToFp64 = "b000_101_111".U
    def mcvtFp64ToFp32 = "b000_111_101".U

    def isCvtDouble (func: UInt) = func(8, 5) === "b001_0".U
    def mcvtDoubleWidth = "b001_0_00000".U
    def isCvtHalf   (func: UInt) = func(8, 6) === "b001_1".U
    def mcvtHalfWidth   = "b001_1_00000".U

    // 4.5.1 Data Move between Matrix Registers
    def isMove (func: UInt) = func(8, 6) === "b010".U
    def isMoveFromA (func: UInt) = func(5) === "b0".U
    def isMoveFromT (func: UInt) = func(5) === "b1".U
    def isMoveToA (func: UInt) = func(4) === "b0".U
    def isMoveToT (func: UInt) = func(4) === "b1".U
    // The same as the broadcast
    // def isWidth8  (func: UInt) = func(1, 0) === "b00".U
    // def isWidth16 (func: UInt) = func(1, 0) === "b01".U
    // def isWidth32 (func: UInt) = func(1, 0) === "b10".U
    // def isWidth64 (func: UInt) = func(1, 0) === "b11".U

    def mmove8AA  = "b010_00_00_00".U
    def mmove16AA = "b010_00_00_01".U
    def mmove32AA = "b010_00_00_10".U
    def mmove64AA = "b010_00_00_11".U
    def mmove8TT  = "b010_11_00_00".U
    def mmove16TT = "b010_11_00_01".U
    def mmove32TT = "b010_11_00_10".U
    def mmove64TT = "b010_11_00_11".U

    def mmove8AT   = "b010_10_00_00".U
    def mmove16AT  = "b010_10_00_01".U
    def mmove32AT  = "b010_10_00_10".U
    def mmove64AT  = "b010_10_00_11".U
    def mmove8TA   = "b010_01_00_00".U
    def mmove16TA  = "b010_01_00_01".U
    def mmove32TA  = "b010_01_00_10".U
    def mmove64TA  = "b010_01_00_11".U

    // 4.4.4 Data Broadcast
    def isBroadcast (func: UInt) = func(8, 6) === "b011".U

    // Broadcast the first row of a matrix register to fill the whole matrix.
    def isBroadcastFromRow (func: UInt) = func(5, 4) === "b00".U
    // Broadcast the first column of a matrix register to fill the whole matrix.
    def isBroadcastFromCol (func: UInt) = func(5, 4) === "b01".U
    // Broadcast the first element of a matrix register to fill the whole matrix.
    def isBroadcastFromEle (func: UInt) = func(5, 4) === "b10".U
    
    def isBroadcastFromA (func: UInt) = func(3, 2) === "b00".U
    def isBroadcastFromB (func: UInt) = func(3, 2) === "b01".U
    def isBroadcastFromC (func: UInt) = func(3, 2) === "b10".U
    
    def isWidth8  (func: UInt) = func(1, 0) === "b00".U
    def isWidth16 (func: UInt) = func(1, 0) === "b01".U
    def isWidth32 (func: UInt) = func(1, 0) === "b10".U
    def isWidth64 (func: UInt) = func(1, 0) === "b11".U

    def mbcARow = "b011_00_00_00".U
    def mbcBRow = "b011_00_01_00".U
    def mbcCRow = "b011_00_10_00".U
    
    def mbcACol8  = "b011_01_00_00".U
    def mbcACol16 = "b011_01_00_01".U
    def mbcACol32 = "b011_01_00_10".U
    def mbcACol64 = "b011_01_00_11".U
    def mbcBCol8  = "b011_01_01_00".U
    def mbcBCol16 = "b011_01_01_01".U
    def mbcBCol32 = "b011_01_01_10".U
    def mbcBCol64 = "b011_01_01_11".U
    def mbcCCol8  = "b011_01_10_00".U
    def mbcCCol16 = "b011_01_10_01".U
    def mbcCCol32 = "b011_01_10_10".U
    def mbcCCol64 = "b011_01_10_11".U

    def mbcAEle8  = "b011_10_00_00".U
    def mbcAEle16 = "b011_10_00_01".U
    def mbcAEle32 = "b011_10_00_10".U
    def mbcAEle64 = "b011_10_00_11".U
    def mbcBEle8  = "b011_10_01_00".U
    def mbcBEle16 = "b011_10_01_01".U
    def mbcBEle32 = "b011_10_01_10".U
    def mbcBEle64 = "b011_10_01_11".U
    def mbcCEle8  = "b011_10_10_00".U
    def mbcCEle16 = "b011_10_10_01".U
    def mbcCEle32 = "b011_10_10_10".U
    def mbcCEle64 = "b011_10_10_11".U

    // 4.4.5 Matrix Transpose
    def isTranspose (func: UInt) = func(8, 4) === "b100_00".U
    // The same as the broadcast
    // def isFromA   (func: UInt) = func(3, 2) === "b00".U
    // def isFromB   (func: UInt) = func(3, 2) === "b01".U
    // def isFromC   (func: UInt) = func(3, 2) === "b10".U
    // def isWidth8  (func: UInt) = func(1, 0) === "b00".U
    // def isWidth16 (func: UInt) = func(1, 0) === "b01".U
    // def isWidth32 (func: UInt) = func(1, 0) === "b10".U
    // def isWidth64 (func: UInt) = func(1, 0) === "b11".U
    def mtransA8  = "b100_00_00_00".U
    def mtransA16 = "b100_00_00_01".U
    def mtransA32 = "b100_00_00_10".U
    def mtransA64 = "b100_00_00_11".U
    def mtransB8  = "b100_00_01_00".U
    def mtransB16 = "b100_00_01_01".U
    def mtransB32 = "b100_00_01_10".U
    def mtransB64 = "b100_00_01_11".U
    def mtransC8  = "b100_00_10_00".U
    def mtransC16 = "b100_00_10_01".U
    def mtransC32 = "b100_00_10_10".U
    def mtransC64 = "b100_00_10_11".U

    // 4.5.2 Element-Wise Instructions
    // TODO: bit-wise logic and element-wise shift for integer matrix
    // def isIntElewiseInst (func: UInt) = func(8, 6) === "b101".U
    // def isAnd  (func: UInt) = func(5, 3) === "b000".U
    // def isOr   (func: UInt) = func(5, 3) === "b001".U
    // def isXor  (func: UInt) = func(5, 3) === "b010".U
    // def isSll  (func: UInt) = func(5, 3) === "b011".U
    // def isSrl  (func: UInt) = func(5, 3) === "b100".U
    // def isSra  (func: UInt) = func(5, 3) === "b101".U
    // def isAddu (func: UInt) = func(5, 3) === "b110".U
    // def isSubu (func: UInt) = func(5, 3) === "b111".U

    // The same as the broadcast
    // def isWidth8  (func: UInt) = func(1, 0) === "b00".U
    // def isWidth16 (func: UInt) = func(1, 0) === "b01".U
    // def isWidth32 (func: UInt) = func(1, 0) === "b10".U
    // def isWidth64 (func: UInt) = func(1, 0) === "b11".U

    def isIntFpElewiseInst (func: UInt) = func(8, 7) === "b11".U
    
    def isIntElewise (func: UInt) = func(6) === "b0".U
    def isFpElewise  (func: UInt) = func(6) === "b1".U
    
    def isAdd  (func: UInt) = func(5, 3) === "b000".U
    def isSub  (func: UInt) = func(5, 3) === "b001".U
    def isMul  (func: UInt) = func(5, 3) === "b010".U
    def isDiv  (func: UInt) = func(5, 3) === "b011".U
    def isMax  (func: UInt) = func(5, 3) === "b100".U
    def isMin  (func: UInt) = func(5, 3) === "b101".U
    def isSqrt (func: UInt) = func(5, 3) === "b110".U

    def isDoubleWiden (func: UInt) = func(2) === "b1".U
    
    // The same as the broadcast
    // def isWidth8  (func: UInt) = func(1, 0) === "b00".U
    // def isWidth16 (func: UInt) = func(1, 0) === "b01".U
    // def isWidth32 (func: UInt) = func(1, 0) === "b10".U
    // def isWidth64 (func: UInt) = func(1, 0) === "b11".U
    
    def mfadd8   = "b111_000_0_00".U
    def mfadd16  = "b111_000_0_01".U
    def mfadd32  = "b111_000_0_10".U
    def mfadd64  = "b111_000_0_11".U
    def mfwadd8  = "b111_000_1_00".U
    def mfwadd16 = "b111_000_1_01".U
    def mfwadd32 = "b111_000_1_10".U
    def mfsub8   = "b111_001_0_00".U
    def mfsub16  = "b111_001_0_01".U
    def mfsub32  = "b111_001_0_10".U
    def mfsub64  = "b111_001_0_11".U
    def mfwsub8  = "b111_001_1_00".U
    def mfwsub16 = "b111_001_1_01".U
    def mfwsub32 = "b111_001_1_10".U
    def mfmul8   = "b111_010_0_00".U
    def mfmul16  = "b111_010_0_01".U
    def mfmul32  = "b111_010_0_10".U
    def mfmul64  = "b111_010_0_11".U
    def mfwmul8  = "b111_010_1_00".U
    def mfwmul16 = "b111_010_1_01".U
    def mfwmul32 = "b111_010_1_10".U
    def mfdiv8   = "b111_011_0_00".U
    def mfdiv16  = "b111_011_0_01".U
    def mfdiv32  = "b111_011_0_10".U
    def mfdiv64  = "b111_011_0_11".U
    def mfmax8   = "b111_100_0_00".U
    def mfmax16  = "b111_100_0_01".U
    def mfmax32  = "b111_100_0_10".U
    def mfmax64  = "b111_100_0_11".U
    def mfmin8   = "b111_101_0_00".U
    def mfmin16  = "b111_101_0_01".U
    def mfmin32  = "b111_101_0_10".U
    def mfmin64  = "b111_101_0_11".U
    def mfsqrt8  = "b111_110_0_00".U
    def mfsqrt16 = "b111_110_0_01".U
    def mfsqrt32 = "b111_110_0_10".U
    def mfsqrt64 = "b111_110_0_11".U
  }

  object MmvefOpType {
    def placeholder = "b111_111_111".U
  }

  object BRUOpType {
    // branch
    def beq        = "b000_000".U
    def bne        = "b000_001".U
    def blt        = "b000_100".U
    def bge        = "b000_101".U
    def bltu       = "b001_000".U
    def bgeu       = "b001_001".U

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object MULOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    def mulw7  = "b01100".U
    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(1, 0) =/= 0.U
    def getOp(op: UInt) = Cat(op(3), op(1, 0))
  }

  object DIVOpType {
    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b10000".U
    def divu   = "b10010".U
    def rem    = "b10001".U
    def remu   = "b10011".U

    def divw   = "b10100".U
    def divuw  = "b10110".U
    def remw   = "b10101".U
    def remuw  = "b10111".U

    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(0)
  }

  object MDUOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    def mulw7  = "b01100".U

    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b10000".U
    def divu   = "b10010".U
    def rem    = "b10001".U
    def remu   = "b10011".U

    def divw   = "b10100".U
    def divuw  = "b10110".U
    def remw   = "b10101".U
    def remuw  = "b10111".U

    def isMul(op: UInt) = !op(4)
    def isDiv(op: UInt) = op(4)

    def isDivSign(op: UInt) = isDiv(op) && !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = (isDiv(op) && op(0)) || (isMul(op) && op(1, 0) =/= 0.U)
    def getMulOp(op: UInt) = op(1, 0)
  }

  object LSUOpType {
    // The max length is 6 bits
    // load pipeline

    // normal load
    // Note: bit(1, 0) are size, DO NOT CHANGE
    // bit encoding: | load 0 | is unsigned(1bit) | size(2bit) |
    def lb       = "b0000".U
    def lh       = "b0001".U
    def lw       = "b0010".U
    def ld       = "b0011".U
    def lbu      = "b0100".U
    def lhu      = "b0101".U
    def lwu      = "b0110".U
    // hypervior load
    // bit encoding: | hlv 1 | hlvx 1 | is unsigned(1bit) | size(2bit) |
    def hlvb   = "b10000".U
    def hlvh   = "b10001".U
    def hlvw   = "b10010".U
    def hlvd   = "b10011".U
    def hlvbu  = "b10100".U
    def hlvhu  = "b10101".U
    def hlvwu  = "b10110".U
    def hlvxhu = "b11101".U
    def hlvxwu = "b11110".U
    def isHlv(op: UInt): Bool = op(4) && (op(5) === "b0".U) && (op(8, 7) === "b00".U)
    def isHlvx(op: UInt): Bool = op(4) && op(3) && (op(5) === "b0".U) && (op(8, 7) === "b00".U)

    // Zicbop software prefetch
    // bit encoding: | prefetch 1 | 0 | prefetch type (2bit) |
    def prefetch_i = "b1000".U // TODO
    def prefetch_r = "b1001".U
    def prefetch_w = "b1010".U

    def isPrefetch(op: UInt): Bool = op(3) && (op(5, 4) === "b000".U) && (op(8, 7) === "b00".U)

    // store pipeline
    // normal store
    // bit encoding: | store 00 | size(2bit) |
    def sb       = "b0000".U
    def sh       = "b0001".U
    def sw       = "b0010".U
    def sd       = "b0011".U

    //hypervisor store
    // bit encoding: |hsv 1 | store 00 | size(2bit) |
    def hsvb = "b10000".U
    def hsvh = "b10001".U
    def hsvw = "b10010".U
    def hsvd = "b10011".U
    def isHsv(op: UInt): Bool = op(4) && (op(5) === "b0".U) && (op(8, 7) === "b00".U)
    // l1 cache op
    // bit encoding: | cbo_zero 01 | size(2bit) 11 |
    def cbo_zero  = "b0111".U

    // llc op
    // bit encoding: | prefetch 11 | suboptype(2bit) |
    def cbo_clean = "b1100".U
    def cbo_flush = "b1101".U
    def cbo_inval = "b1110".U

    def isCbo(op: UInt): Bool = op(3, 2) === "b11".U && (op(6, 4) === "b000".U)
    def isCboAll(op: UInt): Bool = isCbo(op) || op(3,0) === cbo_zero
    def isCboClean(op: UInt): Bool = isCbo(op) && (op(3, 0) === cbo_clean)
    def isCboFlush(op: UInt): Bool = isCbo(op) && (op(3, 0) === cbo_flush)
    def isCboInval(op: UInt): Bool = isCbo(op) && (op(3, 0) === cbo_inval)

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    // bit encoding: | optype(4bit) | size (2bit) |
    def AMOFuOpWidth = 6
    def lr_w      = "b000010".U
    def sc_w      = "b000110".U
    def amoswap_w = "b001010".U
    def amoadd_w  = "b001110".U
    def amoxor_w  = "b010010".U
    def amoand_w  = "b010110".U
    def amoor_w   = "b011010".U
    def amomin_w  = "b011110".U
    def amomax_w  = "b100010".U
    def amominu_w = "b100110".U
    def amomaxu_w = "b101010".U
    def amocas_w  = "b101110".U

    def lr_d      = "b000011".U
    def sc_d      = "b000111".U
    def amoswap_d = "b001011".U
    def amoadd_d  = "b001111".U
    def amoxor_d  = "b010011".U
    def amoand_d  = "b010111".U
    def amoor_d   = "b011011".U
    def amomin_d  = "b011111".U
    def amomax_d  = "b100011".U
    def amominu_d = "b100111".U
    def amomaxu_d = "b101011".U
    def amocas_d  = "b101111".U

    def amocas_q  = "b101100".U

    def size(op: UInt) = op(1,0)

    def getVecLSMop(fuOpType: UInt): UInt = fuOpType(6, 5)

    def isAllUS   (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && (fuOpType(8) ^ fuOpType(7))// Unit-Stride Whole Masked
    def isUStride (fuOpType: UInt): Bool = fuOpType(6, 0) === "b00_00000".U && (fuOpType(8) ^ fuOpType(7))
    def isWhole   (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01000".U && (fuOpType(8) ^ fuOpType(7))
    def isMasked  (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01011".U && (fuOpType(8) ^ fuOpType(7))
    def isStrided (fuOpType: UInt): Bool = fuOpType(6, 5) === "b10".U && (fuOpType(8) ^ fuOpType(7))
    def isIndexed (fuOpType: UInt): Bool = fuOpType(5) && (fuOpType(8) ^ fuOpType(7))
    def isLr      (fuOpType: UInt): Bool = fuOpType === lr_w || fuOpType === lr_d
    def isSc      (fuOpType: UInt): Bool = fuOpType === sc_w || fuOpType === sc_d
    def isAMOCASQ (fuOpType: UInt): Bool = fuOpType === amocas_q
    def isAMOCASWD(fuOpType: UInt): Bool = fuOpType === amocas_w || fuOpType === amocas_d
    def isAMOCAS  (fuOpType: UInt): Bool = fuOpType(5, 2) === "b1011".U
  }

  object BKUOpType {

    def clmul       = "b000000".U
    def clmulh      = "b000001".U
    def clmulr      = "b000010".U
    def xpermn      = "b000100".U
    def xpermb      = "b000101".U

    def clz         = "b001000".U
    def clzw        = "b001001".U
    def ctz         = "b001010".U
    def ctzw        = "b001011".U
    def cpop        = "b001100".U
    def cpopw       = "b001101".U

    // 01xxxx is reserve
    def aes64es     = "b100000".U
    def aes64esm    = "b100001".U
    def aes64ds     = "b100010".U
    def aes64dsm    = "b100011".U
    def aes64im     = "b100100".U
    def aes64ks1i   = "b100101".U
    def aes64ks2    = "b100110".U

    // merge to two instruction sm4ks & sm4ed
    def sm4ed0      = "b101000".U
    def sm4ed1      = "b101001".U
    def sm4ed2      = "b101010".U
    def sm4ed3      = "b101011".U
    def sm4ks0      = "b101100".U
    def sm4ks1      = "b101101".U
    def sm4ks2      = "b101110".U
    def sm4ks3      = "b101111".U

    def sha256sum0  = "b110000".U
    def sha256sum1  = "b110001".U
    def sha256sig0  = "b110010".U
    def sha256sig1  = "b110011".U
    def sha512sum0  = "b110100".U
    def sha512sum1  = "b110101".U
    def sha512sig0  = "b110110".U
    def sha512sig1  = "b110111".U

    def sm3p0       = "b111000".U
    def sm3p1       = "b111001".U
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object SelImm {
    def IMM_X  = "b00111".U
    def IMM_S  = "b01110".U
    def IMM_SB = "b00001".U
    def IMM_U  = "b00010".U
    def IMM_UJ = "b00011".U
    def IMM_I  = "b00100".U
    def IMM_Z  = "b00101".U
    def INVALID_INSTR = "b00110".U
    def IMM_B6 = "b01000".U

    def IMM_OPIVIS = "b01001".U
    def IMM_OPIVIU = "b01010".U
    def IMM_VSETVLI   = "b01100".U
    def IMM_VSETIVLI  = "b01101".U
    def IMM_LUI32 = "b01011".U
    def IMM_VRORVI = "b01111".U

    def IMM_MSET = "b10000".U
    def IMM_MSETVAL = "b10001".U
    def IMM_MSETFIELD = "b10010".U
    def IMM_MATRIXREG = "b10011".U

    def X      = BitPat("b00000")

    def apply() = UInt(5.W)

    def mkString(immType: UInt) : String = {
      val strMap = Map(
        IMM_S.litValue         -> "S",
        IMM_SB.litValue        -> "SB",
        IMM_U.litValue         -> "U",
        IMM_UJ.litValue        -> "UJ",
        IMM_I.litValue         -> "I",
        IMM_Z.litValue         -> "Z",
        IMM_B6.litValue        -> "B6",
        IMM_OPIVIS.litValue    -> "VIS",
        IMM_OPIVIU.litValue    -> "VIU",
        IMM_VSETVLI.litValue   -> "VSETVLI",
        IMM_VSETIVLI.litValue  -> "VSETIVLI",
        IMM_LUI32.litValue     -> "LUI32",
        IMM_VRORVI.litValue    -> "VRORVI",
        INVALID_INSTR.litValue -> "INVALID",
        IMM_MSET.litValue      -> "MSET",
        IMM_MSETVAL.litValue   -> "MSETVAL",
        IMM_MSETFIELD.litValue -> "MSETFIELD",
        IMM_MATRIXREG.litValue -> "MATRIXREG",
      )
      strMap(immType.litValue)
    }

    def getImmUnion(immType: UInt) : Imm = {
      val iuMap = Map(
        IMM_S.litValue         -> ImmUnion.S,
        IMM_SB.litValue        -> ImmUnion.B,
        IMM_U.litValue         -> ImmUnion.U,
        IMM_UJ.litValue        -> ImmUnion.J,
        IMM_I.litValue         -> ImmUnion.I,
        IMM_Z.litValue         -> ImmUnion.Z,
        IMM_B6.litValue        -> ImmUnion.B6,
        IMM_OPIVIS.litValue    -> ImmUnion.OPIVIS,
        IMM_OPIVIU.litValue    -> ImmUnion.OPIVIU,
        IMM_VSETVLI.litValue   -> ImmUnion.VSETVLI,
        IMM_VSETIVLI.litValue  -> ImmUnion.VSETIVLI,
        IMM_LUI32.litValue     -> ImmUnion.LUI32,
        IMM_VRORVI.litValue    -> ImmUnion.VRORVI,
        IMM_MSET.litValue      -> ImmUnion.MSET,
        IMM_MSETVAL.litValue   -> ImmUnion.MSETVAL,
        IMM_MSETFIELD.litValue -> ImmUnion.MSETFIELD,
        IMM_MATRIXREG.litValue -> ImmUnion.MATRIXREG,
      )
      iuMap(immType.litValue)
    }
  }

  object UopSplitType {
    def SCA_SIM          = "b0000000".U //
    def VSET             = "b0010001".U // dirty: vset
    def VEC_VVV          = "b0010010".U // VEC_VVV
    def VEC_VXV          = "b0010011".U // VEC_VXV
    def VEC_0XV          = "b0010100".U // VEC_0XV
    def VEC_VVW          = "b0010101".U // VEC_VVW
    def VEC_WVW          = "b0010110".U // VEC_WVW
    def VEC_VXW          = "b0010111".U // VEC_VXW
    def VEC_WXW          = "b0011000".U // VEC_WXW
    def VEC_WVV          = "b0011001".U // VEC_WVV
    def VEC_WXV          = "b0011010".U // VEC_WXV
    def VEC_EXT2         = "b0011011".U // VF2 0 -> V
    def VEC_EXT4         = "b0011100".U // VF4 0 -> V
    def VEC_EXT8         = "b0011101".U // VF8 0 -> V
    def VEC_VVM          = "b0011110".U // VEC_VVM
    def VEC_VXM          = "b0011111".U // VEC_VXM
    def VEC_SLIDE1UP     = "b0100000".U // vslide1up.vx
    def VEC_FSLIDE1UP    = "b0100001".U // vfslide1up.vf
    def VEC_SLIDE1DOWN   = "b0100010".U // vslide1down.vx
    def VEC_FSLIDE1DOWN  = "b0100011".U // vfslide1down.vf
    def VEC_VRED         = "b0100100".U // VEC_VRED
    def VEC_SLIDEUP      = "b0100101".U // VEC_SLIDEUP
    def VEC_SLIDEDOWN    = "b0100111".U // VEC_SLIDEDOWN
    def VEC_M0X          = "b0101001".U // VEC_M0X  0MV
    def VEC_MVV          = "b0101010".U // VEC_MVV  VMV
    def VEC_VWW          = "b0101100".U //
    def VEC_RGATHER      = "b0101101".U // vrgather.vv, vrgather.vi
    def VEC_RGATHER_VX   = "b0101110".U // vrgather.vx
    def VEC_RGATHEREI16  = "b0101111".U // vrgatherei16.vv
    def VEC_COMPRESS     = "b0110000".U // vcompress.vm
    def VEC_US_LDST      = "b0110001".U // vector unit-strided load/store
    def VEC_S_LDST       = "b0110010".U // vector strided load/store
    def VEC_I_LDST       = "b0110011".U // vector indexed load/store
    def VEC_US_FF_LD     = "b0110100".U // vector unit-stride fault-only-first load
    def VEC_VFV          = "b0111000".U // VEC_VFV
    def VEC_VFW          = "b0111001".U // VEC_VFW
    def VEC_WFW          = "b0111010".U // VEC_WVW
    def VEC_VFM          = "b0111011".U // VEC_VFM
    def VEC_VFRED        = "b0111100".U // VEC_VFRED
    def VEC_VFREDOSUM    = "b0111101".U // VEC_VFREDOSUM
    def VEC_MVNR         = "b0000100".U // vmvnr

    def AMO_CAS_W        = "b0110101".U // amocas_w
    def AMO_CAS_D        = "b0110110".U // amocas_d
    def AMO_CAS_Q        = "b0110111".U // amocas_q

    def MSETTILEX        = "b1000001".U // msettilex
    def MSETTYPE         = "b1000010".U // msettype
    def MAT_MEM          = "b1100001".U // matrix load/store
    def MAT_MUL          = "b1100010".U // matrix multiply
    def MAT_ARITH        = "b1100011".U // matrix arithmetic
    def MAT_CVT          = "b1100100".U // matrix type convert
    def MAT_MBC          = "b1100101".U // matrix broadcast
    // dummy means that the instruction is a complex instruction but uop number is 1
    def dummy     = "b1111111".U

    def X = BitPat("b00000000")

    def apply() = UInt(7.W)
    def needSplit(UopSplitType: UInt) = UopSplitType(4) || UopSplitType(5)

    def isAMOCAS(UopSplitType: UInt): Bool = UopSplitType === AMO_CAS_W || UopSplitType === AMO_CAS_D || UopSplitType === AMO_CAS_Q

    def isVEC(UopSplitType: UInt): Bool = UopSplitType(6) === "b0".U && !isAMOCAS(UopSplitType)
    def isMATRIX(UopSplitType: UInt): Bool = UopSplitType(6) === "b1".U
  }

  object ExceptionNO {
    def instrAddrMisaligned = 0
    def instrAccessFault    = 1
    def illegalInstr        = 2
    def breakPoint          = 3
    def loadAddrMisaligned  = 4
    def loadAccessFault     = 5
    def storeAddrMisaligned = 6
    def storeAccessFault    = 7
    def ecallU              = 8
    def ecallS              = 9
    def ecallVS             = 10
    def ecallM              = 11
    def instrPageFault      = 12
    def loadPageFault       = 13
    // def singleStep          = 14
    def storePageFault      = 15
    def doubleTrap          = 16
    def hardwareError       = 19
    def instrGuestPageFault = 20
    def loadGuestPageFault  = 21
    def virtualInstr        = 22
    def storeGuestPageFault = 23

    // Just alias
    def EX_IAM    = instrAddrMisaligned
    def EX_IAF    = instrAccessFault
    def EX_II     = illegalInstr
    def EX_BP     = breakPoint
    def EX_LAM    = loadAddrMisaligned
    def EX_LAF    = loadAccessFault
    def EX_SAM    = storeAddrMisaligned
    def EX_SAF    = storeAccessFault
    def EX_UCALL  = ecallU
    def EX_HSCALL = ecallS
    def EX_VSCALL = ecallVS
    def EX_MCALL  = ecallM
    def EX_IPF    = instrPageFault
    def EX_LPF    = loadPageFault
    def EX_SPF    = storePageFault
    def EX_DT     = doubleTrap
    def EX_IGPF   = instrGuestPageFault
    def EX_LGPF   = loadGuestPageFault
    def EX_VI     = virtualInstr
    def EX_SGPF   = storeGuestPageFault

    def getAddressMisaligned = Seq(EX_IAM, EX_LAM, EX_SAM)

    def getAccessFault = Seq(EX_IAF, EX_LAF, EX_SAF)

    def getPageFault = Seq(EX_IPF, EX_LPF, EX_SPF)

    def getGuestPageFault = Seq(EX_IGPF, EX_LGPF, EX_SGPF)

    def getLSGuestPageFault = Seq(EX_LGPF, EX_SGPF)

    def getFetchFault = Seq(EX_IAM, EX_IAF, EX_IPF)

    def getLoadFault = Seq(EX_LAM, EX_LAF, EX_LPF)

    def getStoreFault = Seq(EX_SAM, EX_SAF, EX_SPF)

    def priorities = Seq(
      doubleTrap,
      breakPoint, // TODO: different BP has different priority
      instrPageFault,
      instrGuestPageFault,
      instrAccessFault,
      illegalInstr,
      virtualInstr,
      instrAddrMisaligned,
      ecallM, ecallS, ecallVS, ecallU,
      storeAddrMisaligned,
      loadAddrMisaligned,
      storePageFault,
      loadPageFault,
      storeGuestPageFault,
      loadGuestPageFault,
      storeAccessFault,
      loadAccessFault,
      hardwareError
    )

    def getHigherExcpThan(excp: Int): Seq[Int] = {
      val idx = this.priorities.indexOf(excp, 0)
      require(idx != -1, s"The irq($excp) does not exists in IntPriority Seq")
      this.priorities.slice(0, idx)
    }

    def all = priorities.distinct.sorted
    def frontendSet = Seq(
      instrAddrMisaligned,
      instrAccessFault,
      illegalInstr,
      instrPageFault,
      instrGuestPageFault,
      virtualInstr,
      breakPoint
    )
    def partialSelect(vec: Vec[Bool], select: Seq[Int]): Vec[Bool] = {
      val new_vec = Wire(ExceptionVec())
      new_vec.foreach(_ := false.B)
      select.foreach(i => new_vec(i) := vec(i))
      new_vec
    }
    def partialSelect(vec: Vec[Bool], select: Seq[Int], unSelect: Seq[Int]): Vec[Bool] = {
      val new_vec = Wire(ExceptionVec())
      new_vec.foreach(_ := false.B)
      select.diff(unSelect).foreach(i => new_vec(i) := vec(i))
      new_vec
    }
    def selectFrontend(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, frontendSet)
    def selectAll(vec: Vec[Bool]): Vec[Bool] = partialSelect(vec, ExceptionNO.all)
    def selectByFu(vec:Vec[Bool], fuConfig: FuConfig): Vec[Bool] =
      partialSelect(vec, fuConfig.exceptionOut)
    def selectByFuAndUnSelect(vec:Vec[Bool], fuConfig: FuConfig, unSelect: Seq[Int]): Vec[Bool] =
      partialSelect(vec, fuConfig.exceptionOut, unSelect)
  }

  object InstSeqNum extends NamedUInt(64)

  object TopDownCounters extends Enumeration {
    val NoStall = Value("NoStall") // Base
    // frontend
    val OverrideBubble = Value("OverrideBubble")
    val FtqUpdateBubble = Value("FtqUpdateBubble")
    // val ControlRedirectBubble = Value("ControlRedirectBubble")
    val TAGEMissBubble = Value("TAGEMissBubble")
    val SCMissBubble = Value("SCMissBubble")
    val ITTAGEMissBubble = Value("ITTAGEMissBubble")
    val RASMissBubble = Value("RASMissBubble")
    val MemVioRedirectBubble = Value("MemVioRedirectBubble")
    val OtherRedirectBubble = Value("OtherRedirectBubble")
    val FtqFullStall = Value("FtqFullStall")

    val ICacheMissBubble = Value("ICacheMissBubble")
    val ITLBMissBubble = Value("ITLBMissBubble")
    val BTBMissBubble = Value("BTBMissBubble")
    val FetchFragBubble = Value("FetchFragBubble")

    // backend
    // long inst stall at rob head
    val DivStall = Value("DivStall") // int div, float div/sqrt
    val IntNotReadyStall = Value("IntNotReadyStall") // int-inst at rob head not issue
    val FPNotReadyStall = Value("FPNotReadyStall") // fp-inst at rob head not issue
    val MemNotReadyStall = Value("MemNotReadyStall") // mem-inst at rob head not issue
    // freelist full
    val IntFlStall = Value("IntFlStall")
    val FpFlStall = Value("FpFlStall")
    val VecFlStall = Value("VecFlStall")
    val V0FlStall = Value("V0FlStall")
    val VlFlStall = Value("VlFlStall")
    val MxFlStall = Value("MxFlStall")
    val MultiFlStall = Value("MultiFlStall")

    // memblock
    val LoadTLBStall = Value("LoadTLBStall")
    val LoadL1Stall = Value("LoadL1Stall")
    val LoadL2Stall = Value("LoadL2Stall")
    val LoadL3Stall = Value("LoadL3Stall")
    val LoadMemStall = Value("LoadMemStall")
    val StoreStall = Value("StoreStall") // include store tlb miss
    val AtomicStall = Value("AtomicStall") // atomic, load reserved, store conditional

    // xs replay (different to gem5)
    val LoadVioReplayStall = Value("LoadVioReplayStall")
    val LoadMSHRReplayStall = Value("LoadMSHRReplayStall")

    // bad speculation
    val ControlRecoveryStall = Value("ControlRecoveryStall")
    val MemVioRecoveryStall = Value("MemVioRecoveryStall")
    val OtherRecoveryStall = Value("OtherRecoveryStall")

    val FlushedInsts = Value("FlushedInsts") // control flushed, memvio flushed, others

    val OtherCoreStall = Value("OtherCoreStall")

    val NumStallReasons = Value("NumStallReasons")
  }
}
