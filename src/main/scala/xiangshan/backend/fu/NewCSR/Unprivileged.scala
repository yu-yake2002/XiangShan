package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import utility.GatedValidRegNext
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.matrix.Bundles._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = WARL(0, wNoFilter)
    val UF = WARL(1, wNoFilter)
    val OF = WARL(2, wNoFilter)
    val DZ = WARL(3, wNoFilter)
    val NV = WARL(4, wNoFilter)
    val FRM = WARL(7, 5, wNoFilter).withReset(0.U)
  }) with HasRobCommitBundle {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRFFlagsBundle)))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRFrmBundle)))
    val fflags = IO(Output(Fflags()))
    val frm = IO(Output(Frm()))
    val fflagsRdata = IO(Output(Fflags()))
    val frmRdata = IO(Output(Frm()))

    for (wAlias <- Seq(wAliasFflags, wAliasFfm)) {
      for ((name, field) <- wAlias.wdataFields.elements) {
        reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
          wAlias.wen && field.asInstanceOf[CSREnumType].isLegal,
          field.asInstanceOf[CSREnumType]
        )
      }
    }

    // write connection
    reconnectReg()

    when (robCommit.fflags.valid) {
      reg.NX := robCommit.fflags.bits(0) || reg.NX
      reg.UF := robCommit.fflags.bits(1) || reg.UF
      reg.OF := robCommit.fflags.bits(2) || reg.OF
      reg.DZ := robCommit.fflags.bits(3) || reg.DZ
      reg.NV := robCommit.fflags.bits(4) || reg.NV
    }

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt

    fflagsRdata := fflags.asUInt
    frmRdata := frm.asUInt
  }).setAddr(CSRs.fcsr)

  // vec
  val vstart = Module(new CSRModule("Vstart", new CSRBundle {
    // vstart is not a WARL CSR.
    // Since we need to judge whether flush pipe by vstart being not 0 in DecodeStage, vstart must be initialized to some value at reset.
    val vstart = RW(VlWidth - 2, 0).withReset(0.U) // hold [0, 128)
  }) with HasRobCommitBundle {
    // Todo make The use of vstart values greater than the largest element index for the current SEW setting is reserved.
    // Not trap
    when (wen) {
      reg.vstart := this.w.wdata(VlWidth - 2, 0)
    }.elsewhen (robCommit.vsDirty && !robCommit.vstart.valid) {
      reg.vstart := 0.U
    }.elsewhen (robCommit.vstart.valid) {
      reg.vstart := robCommit.vstart.bits
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.vstart)

  val vcsr = Module(new CSRModule("Vcsr", new CSRBundle {
    val VXSAT = RW(   0)
    val VXRM  = RW(2, 1)
  }) with HasRobCommitBundle {
    val wAliasVxsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXSAT = RW(0)
    })))
    val wAliasVxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXRM = RW(1, 0)
    })))
    val vxsat = IO(Output(Vxsat()))
    val vxrm  = IO(Output(Vxrm()))

    for (wAlias <- Seq(wAliasVxsat, wAliasVxrm)) {
      for ((name, field) <- wAlias.wdataFields.elements) {
        reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
          wAlias.wen && field.asInstanceOf[CSREnumType].isLegal,
          field.asInstanceOf[CSREnumType]
        )
      }
    }

    // write connection
    reconnectReg()

    when(robCommit.vxsat.valid) {
      reg.VXSAT := reg.VXSAT.asBool || robCommit.vxsat.bits.asBool
    }

    // read connection
    vxsat := reg.VXSAT.asUInt
    vxrm  := reg.VXRM.asUInt
  }).setAddr(CSRs.vcsr)

  val vl = Module(new CSRModule("Vl", new CSRBundle {
    val VL = RO(VlWidth - 1, 0).withReset(0.U)
  }))
    .setAddr(CSRs.vl)

  val vtype = Module(new CSRModule("Vtype", new CSRVTypeBundle) with HasRobCommitBundle {
    when(robCommit.vtype.valid) {
      reg := robCommit.vtype.bits
    }
  })
    .setAddr(CSRs.vtype)

  val vlenb = Module(new CSRModule("Vlenb", new CSRBundle {
    val VLENB = VlenbField(63, 0).withReset(VlenbField.init)
  }))
    .setAddr(CSRs.vlenb)

  val mtype = Module(new CSRModule("Mtype", new CSRMTypeBundle) with HasRobCommitBundle {
    when(robCommit.mtype.valid) {
      reg := robCommit.mtype.bits
    }
  })
    .setAddr(CSRs.mtype)

  // Matrix tile size registers, read-only.
  // They can be updated only by msettilem/n/k instructions.
  val mtilem = Module(new CSRModule("Mtilem", new CSRBundle {
    val MTILEM = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilem)

  val mtilen = Module(new CSRModule("Mtilen", new CSRBundle {
    val MTILEN = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilen)

  val mtilek = Module(new CSRModule("Mtilek", new CSRBundle {
    val MTILEK = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilek)
  
  val mstart = Module(new CSRModule("Mstart", new CSRBundle {
    // mstart's width here references vstart's width
    val MSTART = RW(MlWidth - 2, 0).withReset(0.U)
  }) with HasRobCommitBundle {
    when (wen) {
      reg.MSTART := this.w.wdata(MlWidth - 2, 0)
    }.elsewhen(robCommit.vsDirty && !robCommit.mstart.valid) {
      reg.MSTART := 0.U
    }.elsewhen(robCommit.mstart.valid) {
      reg := robCommit.mstart.bits
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.mstart)

  val mcsr = Module(new CSRModule("Mcsr", new CSRBundle {
    val MSAT = RW(0)
  }))
    .setAddr(CSRs.mcsr)

  val mlenb = Module(new CSRModule("Mlenb", new CSRBundle {
    val MLENB = MlenbField(63, 0).withReset(MlenbField.init)
  }))
    .setAddr(CSRs.mlenb)

  val mrlenb = Module(new CSRModule("Mrlenb", new CSRBundle {
    val MRLENB = MrlenbField(63, 0).withReset(MrlenbField.init)
  }))
    .setAddr(CSRs.mrlenb)

  val mamul = Module(new CSRModule("Mamul", new CSRBundle {
    val MAMUL = MamulField(63, 0).withReset(MamulField.init)
  }))
    .setAddr(CSRs.mamul)

  val cycle = Module(new CSRModule("cycle", new CSRBundle {
    val cycle = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    when(unprivCountUpdate) {
      reg := mHPM.cycle
    }.otherwise{
      reg := reg
    }
    regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.cycle)
  })
    .setAddr(CSRs.cycle)

  val time = Module(new CSRModule("time", new CSRBundle {
    val time = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    val updated = IO(Output(Bool()))
    val stime  = IO(Output(UInt(64.W)))
    val vstime = IO(Output(UInt(64.W)))

    val stimeTmp  = mHPM.time.bits
    val vstimeTmp = mHPM.time.bits + htimedelta

    // Update when rtc clock tick and not dcsr.STOPTIME
    // or virtual mode changed
    // Note: we delay a cycle and use `v` for better timing
    val virtModeChanged = RegNext(nextV =/= v, false.B)
    when(mHPM.time.valid && !debugModeStopTime || virtModeChanged) {
      reg.time := Mux(v, vstimeTmp, stimeTmp)
    }.otherwise {
      reg := reg
    }

    updated := GatedValidRegNext(mHPM.time.valid && !debugModeStopTime)
    stime  := stimeTmp
    vstime := vstimeTmp
  })
    .setAddr(CSRs.time)

  val instret = Module(new CSRModule("instret", new CSRBundle {
    val instret = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    when(unprivCountUpdate) {
      reg := mHPM.instret
    }.otherwise{
      reg := reg
    }
    regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.instret)
  })
    .setAddr(CSRs.instret)

  val hpmcounters: Seq[CSRModule[_]] = (3 to 0x1F).map(num =>
    Module(new CSRModule(s"Hpmcounter$num", new CSRBundle {
      val hpmcounter = RO(63, 0).withReset(0.U)
    }) with HasMHPMSink with HasDebugStopBundle {
      when(unprivCountUpdate) {
        reg := mHPM.hpmcounters(num - 3)
      }.otherwise{
        reg := reg
      }
      regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.hpmcounters(num - 3))
    }).setAddr(CSRs.cycle + num)
  )

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap(
    CSRs.fflags -> (fcsr.wAliasFflags -> fcsr.fflagsRdata),
    CSRs.frm    -> (fcsr.wAliasFfm    -> fcsr.frmRdata),
    CSRs.fcsr   -> (fcsr.w            -> fcsr.rdata),
    CSRs.vstart -> (vstart.w          -> vstart.rdata),
    CSRs.vxsat  -> (vcsr.wAliasVxsat  -> vcsr.vxsat),
    CSRs.vxrm   -> (vcsr.wAliasVxrm   -> vcsr.vxrm),
    CSRs.vcsr   -> (vcsr.w            -> vcsr.rdata),
    CSRs.vl     -> (vl.w              -> vl.rdata),
    CSRs.vtype  -> (vtype.w           -> vtype.rdata),
    CSRs.vlenb  -> (vlenb.w           -> vlenb.rdata),
    CSRs.mtype  -> (mtype.w           -> mtype.rdata),
    CSRs.mtilem -> (mtilem.w          -> mtilem.rdata),
    CSRs.mtilen -> (mtilen.w          -> mtilen.rdata),
    CSRs.mtilek -> (mtilek.w          -> mtilek.rdata),
    CSRs.mlenb  -> (mlenb.w           -> mlenb.rdata),
    CSRs.mrlenb -> (mrlenb.w          -> mrlenb.rdata),
    CSRs.mamul  -> (mamul.w           -> mamul.rdata),
    CSRs.mcsr   -> (mcsr.w            -> mcsr.rdata),
    CSRs.mstart -> (mstart.w          -> mstart.rdata),
    CSRs.cycle  -> (cycle.w           -> cycle.rdata),
    CSRs.time   -> (time.w            -> time.rdata),
    CSRs.instret -> (instret.w        -> instret.rdata),
  ) ++ hpmcounters.map(counter => (counter.addr -> (counter.w -> counter.rdata)))

  val unprivilegedCSRMods: Seq[CSRModule[_]] = Seq(
    fcsr,
    vcsr,
    vstart,
    vl,
    vtype,
    vlenb,
    mtype,
    mtilem,
    mtilen,
    mtilek,
    mlenb,
    mrlenb,
    mamul,
    mcsr,
    mstart,
    cycle,
    time,
    instret,
  ) ++ hpmcounters

  val unprivilegedCSROutMap: SeqMap[Int, UInt] = SeqMap(
    CSRs.fflags  -> fcsr.fflags.asUInt,
    CSRs.frm     -> fcsr.frm.asUInt,
    CSRs.fcsr    -> fcsr.rdata.asUInt,
    CSRs.vstart  -> vstart.rdata.asUInt,
    CSRs.vxsat   -> vcsr.vxsat.asUInt,
    CSRs.vxrm    -> vcsr.vxrm.asUInt,
    CSRs.vcsr    -> vcsr.rdata.asUInt,
    CSRs.vl      -> vl.rdata.asUInt,
    CSRs.vtype   -> vtype.rdata.asUInt,
    CSRs.vlenb   -> vlenb.rdata.asUInt,
    CSRs.mtype   -> mtype.rdata.asUInt,
    CSRs.mtilem  -> mtilem.rdata.asUInt,
    CSRs.mtilen  -> mtilen.rdata.asUInt,
    CSRs.mtilek  -> mtilek.rdata.asUInt,
    CSRs.mlenb   -> mlenb.rdata.asUInt,
    CSRs.mrlenb  -> mrlenb.rdata.asUInt,
    CSRs.mamul   -> mamul.rdata.asUInt,
    CSRs.mcsr    -> mcsr.rdata.asUInt,
    CSRs.mstart  -> mstart.rdata.asUInt,
    CSRs.cycle   -> cycle.rdata,
    CSRs.time    -> time.rdata,
    CSRs.instret -> instret.rdata,
  ) ++ hpmcounters.map(counter => (counter.addr -> counter.rdata))
}

class CSRVTypeBundle extends CSRBundle {
  // vtype's vill is initialized to 1, when executing vector instructions
  // which depend on vtype, will raise illegal instruction exception
  val VILL  = RO(  63).withReset(1.U)
  val VMA   = RO(   7).withReset(0.U)
  val VTA   = RO(   6).withReset(0.U)
  val VSEW  = RO(5, 3).withReset(0.U)
  val VLMUL = RO(2, 0).withReset(0.U)
}

class CSRFrmBundle extends CSRBundle {
  val FRM = WARL(2, 0, wNoFilter)
}

class CSRFFlagsBundle extends CSRBundle {
  val NX = WARL(0, wNoFilter)
  val UF = WARL(1, wNoFilter)
  val OF = WARL(2, wNoFilter)
  val DZ = WARL(3, wNoFilter)
  val NV = WARL(4, wNoFilter)
}

class CSRMTypeBundle extends CSRBundle {
  // mtype's mill is initialized to 1, when executing matrix instructions
  // which depend on mtype, will raise illegal instruction exception
  val MILL   = RO(    63).withReset(1.U)
  val MBA    = RO(    15).withReset(0.U)
  val mfp64  = RO(    14).withReset(0.U)
  val mfp32  = RO(13, 12).withReset(0.U)
  val mfp16  = RO(11, 10).withReset(0.U)
  val mfp8   = RO(  9, 8).withReset(0.U)
  val mint64 = RO(     7).withReset(0.U)
  val mint32 = RO(     6).withReset(0.U)
  val mint16 = RO(     5).withReset(0.U)
  val mint8  = RO(     4).withReset(0.U)
  val mint4  = RO(     3).withReset(0.U)
  val msew   = RO(  2, 0).withReset(0.U)
}

object VlenbField extends CSREnum with ROApply {
  val init = Value((VLEN / 8).U)
}

object MlenbField extends CSREnum with ROApply {
  val init = Value((MLEN / 8).U)
}

object MrlenbField extends CSREnum with ROApply {
  val init = Value((RLEN / 8).U)
}

object MamulField extends CSREnum with ROApply {
  val init = Value(AMUL.U)
}

trait HasMHPMSink { self: CSRModule[_] =>
  val mHPM = IO(Input(new Bundle {
    val cycle   = UInt(64.W)
    // ValidIO is used to update time reg
    val time    = ValidIO(UInt(64.W))
    val instret = UInt(64.W)
    val hpmcounters = Vec(perfCntNum, UInt(XLEN.W))
  }))
  val v = IO(Input(Bool()))
  val nextV = IO(Input(Bool()))
  val htimedelta = IO(Input(UInt(64.W)))
}

trait HasDebugStopBundle { self: CSRModule[_] =>
  val debugModeStopCount = IO(Input(Bool()))
  val debugModeStopTime  = IO(Input(Bool()))
  val unprivCountUpdate  = IO(Input(Bool()))
}