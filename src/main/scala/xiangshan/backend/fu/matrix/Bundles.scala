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

    // TODO: implement me
    def fromInstMType(instMType: InstMType)(implicit p: Parameters) : MType = {
      val res = Wire(MType())
      res.illegal := false.B // Todo: add illegal check function
      res
    }

    def fromMtypeStruct(mtypeStruct: MtypeStruct)(implicit p: Parameters): MType = {
      val res = Wire(MType())
      res
    }

    def toMtypeStruct(mtype: MType)(implicit p: Parameters) : MtypeStruct = {
      val res = WireInit(0.U.asTypeOf(new MtypeStruct))
      res
    }

    def initMtype()(implicit p: Parameters) : MType = {
      val res = Wire(MType())
      res
    }

    // TODO: What's their meaning?
    def mu: UInt = 0.U(1.W)
    def ma: UInt = 1.U(1.W)
    def tu: UInt = 0.U(1.W)
    def ta: UInt = 1.U(1.W)
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
  }

  object MSew extends NamedUInt(2) {
    def e8  : UInt = "b000".U(width.W)
    def e16 : UInt = "b001".U(width.W)
    def e32 : UInt = "b010".U(width.W)
    def e64 : UInt = "b011".U(width.W)

    def reserved: BitPat = BitPat("b1??")

    def isReserved(sew: UInt) : Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        sew === reserved
      } else {
        false.B
      }
    }
  }

  object MSewOH extends NamedUInt(4) {
    def e8  : UInt = "b0001".U(width.W)
    def e16 : UInt = "b0010".U(width.W)
    def e32 : UInt = "b0100".U(width.W)
    def e64 : UInt = "b1000".U(width.W)

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
}
