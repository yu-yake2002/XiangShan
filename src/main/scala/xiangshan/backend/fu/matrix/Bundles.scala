package xiangshan.backend.fu.matrix

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.XSCoreParamsKey
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.MtypeStruct
import utility.ZeroExt
import _root_.utils.NamedUInt
import freechips.rocketchip.tile.XLen

object Bundles {

  /**
    * mtype bundle, should not be used as csr reg
    */
  class MType(implicit p: Parameters) extends Bundle {
    val illegal = Bool()
    val mba     = Bool()
    val mfp64   = Bool()
    val mfp32   = UInt(2.W)
    val mfp16   = UInt(2.W)
    val mfp8    = UInt(2.W)
    val mint64  = Bool()
    val mint32  = Bool()
    val mint16  = Bool()
    val mint8   = Bool()
    val mint4   = Bool()
    val msew    = MSew()
  }

  class MsetMType(implicit p: Parameters) extends XSBundle {
    val illegal  = Bool()
    val reserved = UInt((XLEN - 17).W)
    val mba      = Bool()
    val mfp64    = Bool()
    val mfp32    = UInt(2.W)
    val mfp16    = UInt(2.W)
    val mfp8     = UInt(2.W)
    val mint64   = Bool()
    val mint32   = Bool()
    val mint16   = Bool()
    val mint8    = Bool()
    val mint4    = Bool()
    val msew     = MtypeMSew()
  }

  object MType {
    def apply()(implicit p: Parameters) : MType = {
      new MType
    }

    def fromInstMType(instMType: InstMType)(implicit p: Parameters) : MType = {
      val res = Wire(MType())
      res.mba := instMType.mba
      res.mfp64 := instMType.mfp64
      res.mfp32 := instMType.mfp32
      res.mfp16 := instMType.mfp16
      res.mfp8 := instMType.mfp8
      res.mint64 := instMType.mint64
      res.mint32 := instMType.mint32
      res.mint16 := instMType.mint16
      res.mint8 := instMType.mint8
      res.mint4 := instMType.mint4
      res.msew := instMType.msew
      res.illegal := false.B // Todo: add illegal check function
      res
    }

    def fromMtypeStruct(mtypeStruct: MtypeStruct)(implicit p: Parameters): MType = {
      val res = Wire(MType())
      res.illegal := mtypeStruct.mill
      res.mba := mtypeStruct.mba
      res.mfp64 := mtypeStruct.mfp64
      res.mfp32 := mtypeStruct.mfp32
      res.mfp16 := mtypeStruct.mfp16
      res.mfp8 := mtypeStruct.mfp8
      res.mint64 := mtypeStruct.mint64
      res.mint32 := mtypeStruct.mint32
      res.mint16 := mtypeStruct.mint16
      res.mint8 := mtypeStruct.mint8
      res.mint4 := mtypeStruct.mint4
      res.msew := mtypeStruct.msew
      res
    }

    def toMtypeStruct(mtype: MType)(implicit p: Parameters) : MtypeStruct = {
      val res = WireInit(0.U.asTypeOf(new MtypeStruct))
      res.mill := mtype.illegal
      res.mba := mtype.mba
      res.mfp64 := mtype.mfp64
      res.mfp32 := mtype.mfp32
      res.mfp16 := mtype.mfp16
      res.mfp8 := mtype.mfp8
      res.mint64 := mtype.mint64
      res.mint32 := mtype.mint32
      res.mint16 := mtype.mint16
      res.mint8 := mtype.mint8
      res.mint4 := mtype.mint4
      res.msew := mtype.msew
      res
    }

    def toMsetMType(mtype: MType)(implicit p: Parameters) : MsetMType = {
      val res = Wire(MsetMType())
      res.illegal  := mtype.illegal
      res.reserved := 0.U
      res.mba      := mtype.mba
      res.mfp64    := mtype.mfp64
      res.mfp32    := mtype.mfp32
      res.mfp16    := mtype.mfp16
      res.mfp8     := mtype.mfp8
      res.mint64   := mtype.mint64
      res.mint32   := mtype.mint32
      res.mint16   := mtype.mint16
      res.mint8    := mtype.mint8
      res.mint4    := mtype.mint4
      res.msew     := mtype.msew
      res
    }

    def initMtype()(implicit p: Parameters) : MType = {
      val res = Wire(MType())
      res.illegal := false.B
      res.mba := false.B
      res.mfp64 := false.B
      res.mfp32 := 0.U
      res.mfp16 := 0.U
      res.mfp8 := 0.U
      res.mint64 := false.B
      res.mint32 := false.B
      res.mint16 := false.B
      res.mint8 := false.B
      res.mint4 := false.B
      res.msew := MSew.e8
      res
    }
  }

  object MsetMType {
    def apply()(implicit p: Parameters) : MsetMType = {
      new MsetMType
    }

    def fromInstMType(instMType: InstMType)(implicit p: Parameters) : MsetMType = {
      val res = Wire(MsetMType())
      res.mba := instMType.mba
      res.mfp64 := instMType.mfp64
      res.mfp32 := instMType.mfp32
      res.mfp16 := instMType.mfp16
      res.mfp8 := instMType.mfp8
      res.mint64 := instMType.mint64
      res.mint32 := instMType.mint32
      res.mint16 := instMType.mint16
      res.mint8 := instMType.mint8
      res.mint4 := instMType.mint4
      res.msew := instMType.msew(MSew.width - 1, 0)
      res.illegal := false.B
      res.reserved := instMType.reserved
      res
    }

    def fromMtypeStruct(mtypeStruct: MtypeStruct)(implicit p: Parameters): MsetMType = {
      val res = Wire(MsetMType())
      res.illegal := mtypeStruct.mill
      res.mba := mtypeStruct.mba
      res.mfp64 := mtypeStruct.mfp64
      res.mfp32 := mtypeStruct.mfp32
      res.mfp16 := mtypeStruct.mfp16
      res.mfp8 := mtypeStruct.mfp8
      res.mint64 := mtypeStruct.mint64
      res.mint32 := mtypeStruct.mint32
      res.mint16 := mtypeStruct.mint16
      res.mint8 := mtypeStruct.mint8
      res.mint4 := mtypeStruct.mint4
      res.msew := mtypeStruct.msew
      res.reserved := mtypeStruct.reserved
      res
    }

    def toMType(msettype: MsetMType)(implicit p: Parameters): MType = {
      val res = Wire(MType())
      res.illegal := msettype.illegal
      res.mba := msettype.mba
      res.mfp64 := msettype.mfp64
      res.mfp32 := msettype.mfp32
      res.mfp16 := msettype.mfp16
      res.mfp8 := msettype.mfp8
      res.mint64 := msettype.mint64
      res.mint32 := msettype.mint32
      res.mint16 := msettype.mint16
      res.mint8 := msettype.mint8
      res.mint4 := msettype.mint4
      res.msew := msettype.msew
      res
    }
  }

  object MSew extends NamedUInt(3) {
    def e8  : UInt = "b000".U(width.W)
    def e16 : UInt = "b001".U(width.W)
    def e32 : UInt = "b010".U(width.W)
    def e64 : UInt = "b011".U(width.W)
    def e4  : UInt = "b111".U(width.W)

    def reserved = Seq(BitPat("b100"), BitPat("b101"), BitPat("b110"))

    def isReserved(sew: UInt) : Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        reserved.map(sew === _).reduce(_ || _)
      } else {
        false.B
      }
    }
  }

  object MSewOH extends NamedUInt(8) {
    def e8  : UInt = "b00000001".U(width.W)
    def e16 : UInt = "b00000010".U(width.W)
    def e32 : UInt = "b00000100".U(width.W)
    def e64 : UInt = "b00001000".U(width.W)
    def e4  : UInt = "b10000000".U(width.W)

    def convertFromMSew(msew: UInt): UInt = {
      require(msew.getWidth >= 2 && msew.getWidth <= 3)
      ZeroExt(UIntToOH(msew), this.width)
    }
  }

  object MtypeMSew extends NamedUInt(3)

  object Mstart {
    def apply()(implicit p: Parameters): UInt = UInt(width.W)

    def width(implicit p: Parameters) = p(XSCoreParamsKey).vlWidth - 1
  }
  
  object Msat extends NamedUInt(1)

  object Mtilex {
    def apply()(implicit p: Parameters): UInt = UInt(width.W)

    // TODO: use a correct width
    // The mlwidth is just a placeholder
    def width(implicit p: Parameters) = p(XSCoreParamsKey).mlWidth
  }

  class MConfig(implicit p: Parameters) extends Bundle {
    val mtype = new MType
    val tilem = Mtilex()
    val tilen = Mtilex()
    val tilek = Mtilex()
  }
  
  object MConfig {
    def apply()(implicit p: Parameters) : MConfig = {
      new MConfig()
    }
  }

  class AmuMmaIO(implicit p: Parameters) extends XSBundle {
    val md     = UInt(4.W) // 3 : 0
    val sat    = Bool()    // 4
    val ms1    = UInt(4.W) // 8 : 5
    val ms2    = UInt(4.W) // 12 : 9
    val mtilem = Mtilex()  // 21 : 13
    val mtilen = Mtilex()  // 30 : 22
    val mtilek = Mtilex()  // 39 : 31
    val types  = UInt(3.W) // 42 : 40
    val typed  = UInt(3.W) // 45 : 43
  }

  object AmuMmaIO {
    def apply()(implicit p: Parameters) : AmuMmaIO = {
      new AmuMmaIO()
    }
  }

  class AmuLsuIO(implicit p: Parameters) extends XSBundle {
    // src/dest matrix register
    val ms        = UInt(4.W)         // 3 : 0
    // load(0)/store(1)
    val ls        = Bool()            // 4
    // whether transposed
    val transpose = Bool()            // 5

    val baseAddr  = UInt(PAddrBits.W) // 53 : 6
    val stride    = UInt(PAddrBits.W) // 101 : 54
    
    val row       = Mtilex()          // 110 : 102
    val column    = Mtilex()          // 119 : 111
    val widths    = MtypeMSew()       // 122 : 120
  }

  object AmuLsuIO {
    def apply()(implicit p: Parameters) : AmuLsuIO = {
      new AmuLsuIO()
    }
  }

  class AmuCtrlIO(implicit p: Parameters) extends XSBundle {
    // op: Determine the operation
    // 0: MMA
    // 1: Load/Store
    val op = UInt(1.W)
    
    def isMma() : Bool = op === AmuCtrlIO.mmaOp()
    def isMls() : Bool = op === AmuCtrlIO.mlsOp()
    
    // data: The ctrl signal for op
    val data = UInt(128.W)
  }

  object AmuCtrlIO {
    def apply()(implicit p: Parameters) : AmuCtrlIO = {
      new AmuCtrlIO()
    }

    def mmaOp() : UInt = "b0".U
    def mlsOp() : UInt = "b1".U
  }
}
