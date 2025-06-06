package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.fu.NewCSR.CSRBundles.{Counteren, PrivState}
import xiangshan.backend.fu.NewCSR.CSRDefines._
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter

class CSRPermitModule(implicit p: Parameters) extends Module {
  val io = IO(new CSRPermitIO)

  val xRetPermitMod = Module(new XRetPermitModule)
  val mLevelPermitMod = Module(new MLevelPermitModule)
  val sLevelPermitMod = Module(new SLevelPermitModule)
  val privilegePermitMod = Module(new PrivilegePermitModule)
  val virtualLevelPermitMod = Module(new VirtualLevelPermitModule)
  val indirectCSRPermitMod = Module(new IndirectCSRPermitModule)

  xRetPermitMod.io.in.privState := io.in.privState
  xRetPermitMod.io.in.debugMode := io.in.debugMode
  xRetPermitMod.io.in.xRet      := io.in.xRet
  xRetPermitMod.io.in.status    := io.in.status

  mLevelPermitMod.io.in.csrAccess  := io.in.csrAccess
  mLevelPermitMod.io.in.privState  := io.in.privState
  mLevelPermitMod.io.in.status     := io.in.status
  mLevelPermitMod.io.in.xcounteren := io.in.xcounteren
  mLevelPermitMod.io.in.xenvcfg    := io.in.xenvcfg
  mLevelPermitMod.io.in.xstateen   := io.in.xstateen
  mLevelPermitMod.io.in.aia        := io.in.aia

  sLevelPermitMod.io.in.csrAccess  := io.in.csrAccess
  sLevelPermitMod.io.in.privState  := io.in.privState
  sLevelPermitMod.io.in.xcounteren := io.in.xcounteren
  sLevelPermitMod.io.in.xstateen   := io.in.xstateen

  privilegePermitMod.io.in.csrAccess := io.in.csrAccess
  privilegePermitMod.io.in.privState := io.in.privState
  privilegePermitMod.io.in.debugMode := io.in.debugMode

  virtualLevelPermitMod.io.in.csrAccess   := io.in.csrAccess
  virtualLevelPermitMod.io.in.privState   := io.in.privState
  virtualLevelPermitMod.io.in.status      := io.in.status
  virtualLevelPermitMod.io.in.xcounteren  := io.in.xcounteren
  virtualLevelPermitMod.io.in.xenvcfg     := io.in.xenvcfg
  virtualLevelPermitMod.io.in.xstateen    := io.in.xstateen
  virtualLevelPermitMod.io.in.aia         := io.in.aia

  indirectCSRPermitMod.io.in.csrAccess := io.in.csrAccess
  indirectCSRPermitMod.io.in.privState := io.in.privState
  indirectCSRPermitMod.io.in.aia       := io.in.aia
  indirectCSRPermitMod.io.in.xstateen  := io.in.xstateen

  private val (ren, wen) = (
    io.in.csrAccess.ren,
    io.in.csrAccess.wen,
  )

  private val csrAccess = WireInit(ren || wen)

  val mPermit_EX_II = mLevelPermitMod.io.out.mLevelPermit_EX_II

  val sPermit_EX_II = sLevelPermitMod.io.out.sLevelPermit_EX_II

  val pPermit_EX_II = privilegePermitMod.io.out.privilege_EX_II
  val pPermit_EX_VI = privilegePermitMod.io.out.privilege_EX_VI

  val vPermit_EX_II = virtualLevelPermitMod.io.out.virtualLevelPermit_EX_II
  val vPermit_EX_VI = virtualLevelPermitMod.io.out.virtualLevelPermit_EX_VI

  val indirectPermit_EX_II = indirectCSRPermitMod.io.out.indirectCSR_EX_II
  val indirectPermit_EX_VI = indirectCSRPermitMod.io.out.indirectCSR_EX_VI

  val directPermit_illegal = mPermit_EX_II || sPermit_EX_II || pPermit_EX_II || pPermit_EX_VI || vPermit_EX_II || vPermit_EX_VI

  val csrAccess_EX_II = csrAccess && (
    (mPermit_EX_II || sPermit_EX_II || pPermit_EX_II || vPermit_EX_II) ||
    (!directPermit_illegal && indirectPermit_EX_II)
  )
  val csrAccess_EX_VI = csrAccess && (
    (pPermit_EX_VI || vPermit_EX_VI) ||
    (!directPermit_illegal && indirectPermit_EX_VI)
  )

  val Xret_EX_II = xRetPermitMod.io.out.Xret_EX_II
  val Xret_EX_VI = xRetPermitMod.io.out.Xret_EX_VI

  io.out.EX_II := csrAccess_EX_II || Xret_EX_II
  io.out.EX_VI := !io.out.EX_II && (csrAccess_EX_VI || Xret_EX_VI)

  io.out.hasLegalWen   := wen   && !(io.out.EX_II || io.out.EX_VI)
  io.out.hasLegalMNret := xRetPermitMod.io.out.hasLegalMNret
  io.out.hasLegalMret  := xRetPermitMod.io.out.hasLegalMret
  io.out.hasLegalSret  := xRetPermitMod.io.out.hasLegalSret
  io.out.hasLegalDret  := xRetPermitMod.io.out.hasLegalDret

  io.out.hasLegalWriteFcsr := mLevelPermitMod.io.out.hasLegalWriteFcsr
  io.out.hasLegalWriteVcsr := mLevelPermitMod.io.out.hasLegalWriteVcsr
  io.out.hasLegalWriteMcsr := mLevelPermitMod.io.out.hasLegalWriteMcsr
}

class XRetPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val privState = new PrivState
      val debugMode = Bool()
      val xRet = new xRetIO
      val status = new statusIO
    })
    val out = Output(new Bundle {
      val Xret_EX_II = Bool()
      val Xret_EX_VI = Bool()
      val hasLegalMNret = Bool()
      val hasLegalMret  = Bool()
      val hasLegalSret  = Bool()
      val hasLegalDret  = Bool()
    })
  })

  private val (privState, debugMode) = (
    io.in.privState,
    io.in.debugMode,
  )

  private val (mnret, mret, sret, dret) = (
    io.in.xRet.mnret,
    io.in.xRet.mret,
    io.in.xRet.sret,
    io.in.xRet.dret,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
  )

  private val mnret_EX_II = mnret && !privState.isModeM
  private val mnretIllegal =  mnret_EX_II

  private val mret_EX_II = mret && !privState.isModeM
  private val mretIllegal = mret_EX_II

  private val sret_EX_II = sret && (privState.isModeHU || privState.isModeHS && tsr)
  private val sret_EX_VI = sret && (privState.isModeVU || privState.isModeVS && vtsr)
  private val sretIllegal = sret_EX_II || sret_EX_VI

  private val dret_EX_II = dret && !debugMode
  private val dretIllegal = dret_EX_II

  io.out.Xret_EX_II := mnret_EX_II || mret_EX_II || sret_EX_II || dret_EX_II
  io.out.Xret_EX_VI := sret_EX_VI
  io.out.hasLegalMNret := mnret && !mnretIllegal
  io.out.hasLegalMret  := mret  && !mretIllegal
  io.out.hasLegalSret  := sret  && !sretIllegal
  io.out.hasLegalDret  := dret  && !dretIllegal
}

class MLevelPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val status = new statusIO
      val xcounteren = new xcounterenIO
      val xenvcfg = new xenvcfgIO
      val xstateen = new xstateenIO
      val aia = new aiaIO
    })
    val out = Output(new Bundle {
      val mLevelPermit_EX_II = Bool()
      val hasLegalWriteFcsr = Bool()
      val hasLegalWriteVcsr = Bool()
      val hasLegalWriteMcsr = Bool()
    })
  })

  private val (wen, addr, privState) = (
    io.in.csrAccess.wen,
    io.in.csrAccess.addr,
    io.in.privState,
  )

  private val tvm = io.in.status.tvm

  private val mcounteren = io.in.xcounteren.mcounteren

  private val mstateen0 = io.in.xstateen.mstateen0
  private val mstateen1 = io.in.xstateen.mstateen1
  private val mstateen2 = io.in.xstateen.mstateen2
  private val mstateen3 = io.in.xstateen.mstateen3

  private val mcounterenTM = mcounteren(1)

  private val menvcfg = io.in.xenvcfg.menvcfg

  private val menvcfgSTCE = menvcfg(63)

  private val (sFSIsOff, sVSIsOff, sMSIsOff, sOrVsFSIsOff, sOrVsVSIsOff, sOrVsMSIsOff) = (
    io.in.status.mstatusFSOff,
    io.in.status.mstatusVSOff,
    io.in.status.mstatusMSOff,
    io.in.status.mstatusFSOff || io.in.status.vsstatusFSOff,
    io.in.status.mstatusVSOff || io.in.status.vsstatusVSOff,
    io.in.status.mstatusMSOff || io.in.status.vsstatusMSOff
  )

  private val mvienSEIE = io.in.aia.mvienSEIE

  private val csrIsRO = addr(11, 10) === "b11".U
  private val csrIsHPM = addr >= CSRs.cycle.U && addr <= CSRs.hpmcounter31.U
  private val csrIsFp = Seq(CSRs.fflags, CSRs.frm, CSRs.fcsr).map(_.U === addr).reduce(_ || _)
  private val csrIsVec = Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr, CSRs.vtype).map(_.U === addr).reduce(_ || _)
  private val csrIsWritableVec = Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr).map(_.U === addr).reduce(_ || _)
  private val csrIsMatrix = Seq(CSRs.mstart, CSRs.mtype, CSRs.mcsr).map(_.U === addr).reduce(_ || _)
  private val csrIsWritableMatrix = Seq(CSRs.mstart, CSRs.mcsr).map(_.U === addr).reduce(_ || _)

  private val counterAddr = addr(4, 0) // 32 counters

  private val rwIllegal = csrIsRO && wen

  private val fsEffectiveOff = sFSIsOff && !privState.isVirtual || sOrVsFSIsOff && privState.isVirtual
  private val vsEffectiveOff = sVSIsOff && !privState.isVirtual || sOrVsVSIsOff && privState.isVirtual
  private val msEffectiveOff = sMSIsOff && !privState.isVirtual || sOrVsMSIsOff && privState.isVirtual

  private val fpOff_EX_II  = csrIsFp  && fsEffectiveOff
  private val vecOff_EX_II = csrIsVec && vsEffectiveOff
  private val matrixOff_EX_II = csrIsMatrix && msEffectiveOff

  private val fpVecMatrix_EX_II = fpOff_EX_II || vecOff_EX_II || matrixOff_EX_II

  private val rwStimecmp_EX_II = !privState.isModeM && (!mcounterenTM || !menvcfgSTCE) && (addr === CSRs.vstimecmp.U || addr === CSRs.stimecmp.U)

  private val accessHPM_EX_II = csrIsHPM && !privState.isModeM && !mcounteren(counterAddr)

  private val rwSatp_EX_II = privState.isModeHS && tvm && (addr === CSRs.satp.U || addr === CSRs.hgatp.U)

  private val rwStopei_EX_II = privState.isModeHS && mvienSEIE && (addr === CSRs.stopei.U)

  /**
   * Sm/Ssstateen begin
   */
  // SE bit 63
  private val accessStateen_EX_II = (
    mstateen0.SE0.asBool +: Seq(mstateen1, mstateen2, mstateen3).map(_.SE.asBool)
    ).zipWithIndex.map{ case(se, i) => {
    val csrIsHstateen = addr === (CSRs.hstateen0 + i).U
    val csrIsSstateen = addr === (CSRs.sstateen0 + i).U
    val csrIsStateen = csrIsHstateen || csrIsSstateen
    csrIsStateen && !privState.isModeM && !se
  }}.reduce(_ || _)

  // ENVCFG bit 62
  private val csrIsHenvcfg = addr === CSRs.henvcfg.U
  private val csrIsSenvcfg = addr === CSRs.senvcfg.U
  private val csrIsEnvcfg = csrIsHenvcfg || csrIsSenvcfg
  private val accessEnvcfg_EX_II = csrIsEnvcfg && !privState.isModeM && !mstateen0.ENVCFG.asBool

  // CSRIND bit 60 indirect reg (Sscsrind extensions), this is not implemented
  // csr addr S: [0x150, 0x157]     VS: [0x250, 0x257]
  private val csrIsSi = Ireg.isInSCsrInd(addr)
  private val csrIsVSi = Ireg.isInVSCsrInd(addr)
  private val csrIsIND = csrIsSi || csrIsVSi
  private val accessIND_EX_II = csrIsIND && !privState.isModeM && !mstateen0.CSRIND.asBool

  // AIA bit 59
  private val ssAiaHaddr = Seq(CSRs.hvien.U, CSRs.hvictl.U, CSRs.hviprio1.U, CSRs.hviprio2.U)
  private val ssAiaVSaddr = addr === CSRs.vstopi.U
  private val ssAiaSaddr = addr === CSRs.stopi.U
  private val csrIsAIA = ssAiaHaddr.map(_ === addr).reduce(_ || _) || ssAiaVSaddr || ssAiaSaddr
  private val accessAIA_EX_II = csrIsAIA && !privState.isModeM && !mstateen0.AIA.asBool

  // IMSIC bit 58 (Ssaia extension)
  private val csrIsStopei = addr === CSRs.stopei.U
  private val csrIsVStopei = addr === CSRs.vstopei.U
  private val csrIsTpoie = csrIsStopei || csrIsVStopei
  private val accessTopie_EX_II = csrIsTpoie && !privState.isModeM && !mstateen0.IMSIC.asBool

  // CONTEXT bit 57 context reg (Sdtrig extensions)
  private val csrIsHcontext = addr === CSRs.hcontext.U
  private val csrIsScontext = addr === CSRs.scontext.U
  private val csrIsContext = csrIsHcontext || csrIsScontext
  private val accessContext_EX_II = csrIsContext && !privState.isModeM && !mstateen0.CONTEXT.asBool

  // P1P13 bit 56, Read-only 0

  // Custom bit 0
  // csr addr HVS: [0x6c0, 0x6ff], [0xac0, 0xaff], [0xec0, 0xeff]
  private val csrIsHVSCustom = (addr(11, 10) =/= "b00".U) && (addr(9, 8) === "b10".U) && (addr(7, 6) === "b11".U)
  // [0x5c0, 0x5ff], [0x9c0, 0x9ff], [0xdc0, 0xdff]
  private val csrIsSCustom   = (addr(11, 10) =/= "b00".U) && (addr(9, 8) === "b01".U) && (addr(7, 6) === "b11".U)
  // [0x800, 0x8ff], [0xcc0, 0xcff]
  private val csrIsUCustom   = (addr(11, 8) === "b1000".U) || (addr(11, 6) === "b110011".U)
  private val allCustom      = csrIsHVSCustom || csrIsSCustom || csrIsUCustom
  private val accessCustom_EX_II = allCustom && !privState.isModeM && !mstateen0.C.asBool

  private val xstateControlAccess_EX_II = accessStateen_EX_II || accessEnvcfg_EX_II || accessIND_EX_II || accessAIA_EX_II ||
    accessTopie_EX_II || accessContext_EX_II || accessCustom_EX_II
  /**
   * Sm/Ssstateen end
   */

  io.out.mLevelPermit_EX_II := rwIllegal || fpVecMatrix_EX_II || rwStimecmp_EX_II ||
    accessHPM_EX_II || rwSatp_EX_II || rwStopei_EX_II || xstateControlAccess_EX_II
  io.out.hasLegalWriteFcsr := wen && csrIsFp && !fsEffectiveOff
  io.out.hasLegalWriteVcsr := wen && csrIsWritableVec && !vsEffectiveOff
  io.out.hasLegalWriteMcsr := wen && csrIsWritableMatrix && !msEffectiveOff
}

class SLevelPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val xcounteren = new xcounterenIO
      val xstateen = new xstateenIO
    })
    val out = Output(new Bundle {
      val sLevelPermit_EX_II = Bool()
    })
  })

  private val (addr, privState) = (
    io.in.csrAccess.addr,
    io.in.privState,
  )

  private val scounteren = io.in.xcounteren.scounteren

  private val sstateen0 = io.in.xstateen.sstateen0

  private val csrIsHPM = addr >= CSRs.cycle.U && addr <= CSRs.hpmcounter31.U
  private val counterAddr = addr(4, 0) // 32 counters

  private val accessHPM_EX_II = csrIsHPM && privState.isModeHU && !scounteren(counterAddr)

  private val csrIsUCustom   = (addr(11, 8) === "b1000".U) || (addr(11, 6) === "b110011".U)
  private val accessCustom_EX_II = csrIsUCustom && privState.isModeHU && !sstateen0.C.asBool

  io.out.sLevelPermit_EX_II := accessHPM_EX_II || accessCustom_EX_II
}

class PrivilegePermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val debugMode = Bool()
    })
    val out = Output(new Bundle {
      val privilege_EX_II = Bool()
      val privilege_EX_VI = Bool()
    })
  })

  private val (addr, privState, debugMode) = (
    io.in.csrAccess.addr,
    io.in.privState,
    io.in.debugMode
  )

  private val accessTable = TruthTable(Seq(
    //       V PRVM ADDR
    BitPat("b0__00___00") -> BitPat.Y(), // HU access U
    BitPat("b1__00___00") -> BitPat.Y(), // VU access U
    BitPat("b0__01___00") -> BitPat.Y(), // HS access U
    BitPat("b0__01___01") -> BitPat.Y(), // HS access S
    BitPat("b0__01___10") -> BitPat.Y(), // HS access H
    BitPat("b1__01___00") -> BitPat.Y(), // VS access U
    BitPat("b1__01___01") -> BitPat.Y(), // VS access S
    BitPat("b0__11___00") -> BitPat.Y(), // M  access HU
    BitPat("b0__11___01") -> BitPat.Y(), // M  access HS
    BitPat("b0__11___10") -> BitPat.Y(), // M  access H
    BitPat("b0__11___11") -> BitPat.Y(), // M  access M
  ), BitPat.N())

  private val regularPrivilegeLegal = chisel3.util.experimental.decode.decoder(
    privState.V.asUInt ## privState.PRVM.asUInt ## addr(9, 8),
    accessTable
  ).asBool

  private val csrIsM = addr(9, 8) === "b11".U
  private val isDebugReg   = addr(11, 4) === "h7b".U
  private val privilegeLegal = Mux(isDebugReg, debugMode, regularPrivilegeLegal || debugMode)

  io.out.privilege_EX_II := !privilegeLegal && (!privState.isVirtual || csrIsM)
  io.out.privilege_EX_VI := !privilegeLegal && privState.isVirtual && !csrIsM
}

class VirtualLevelPermitModule(implicit val p: Parameters) extends Module with HasSoCParameter {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val status = new statusIO
      val xcounteren = new xcounterenIO
      val xenvcfg = new xenvcfgIO
      val xstateen = new xstateenIO
      val aia = new aiaIO
    })
    val out = Output(new Bundle {
      val virtualLevelPermit_EX_II = Bool()
      val virtualLevelPermit_EX_VI = Bool()
    })
  })

  private val (wen, addr, privState) = (
    io.in.csrAccess.wen,
    io.in.csrAccess.addr,
    io.in.privState,
  )

  private val (vtvm, vgein) = (
    io.in.status.vtvm,
    io.in.status.vgein,
  )

  private val (hcounteren, scounteren) = (
    io.in.xcounteren.hcounteren,
    io.in.xcounteren.scounteren,
  )

  private val hcounterenTM = hcounteren(1)

  private val henvcfg = io.in.xenvcfg.henvcfg

  private val henvcfgSTCE = henvcfg(63)

  private val (hstateen0, hstateen1, hstateen2, hstateen3, sstateen0) = (
    io.in.xstateen.hstateen0,
    io.in.xstateen.hstateen1,
    io.in.xstateen.hstateen2,
    io.in.xstateen.hstateen3,
    io.in.xstateen.sstateen0,
  )

  private val hvictlVTI = io.in.aia.hvictlVTI

  private val csrIsHPM = addr >= CSRs.cycle.U && addr <= CSRs.hpmcounter31.U
  private val counterAddr = addr(4, 0) // 32 counters

  private val rwSatp_EX_VI = privState.isModeVS && vtvm && (addr === CSRs.satp.U)

  private val rwVStopei_EX_II = (privState.isModeM || privState.isModeHS) && (addr === CSRs.vstopei.U) && (vgein === 0.U || vgein > soc.IMSICParams.geilen.U)
  private val rwStopei_EX_VI = privState.isModeVS && (addr === CSRs.stopei.U) && (vgein === 0.U || vgein > soc.IMSICParams.geilen.U)

  private val rwSip_Sie_EX_VI = privState.isModeVS && hvictlVTI && (addr === CSRs.sip.U || addr === CSRs.sie.U)

  private val rwStimecmp_EX_VI = privState.isModeVS && (addr === CSRs.stimecmp.U) &&
    (!hcounterenTM || !henvcfgSTCE || wen && hvictlVTI)

  private val accessHPM_EX_VI = csrIsHPM && (
      privState.isModeVS && !hcounteren(counterAddr) ||
      privState.isModeVU && (!hcounteren(counterAddr) || !scounteren(counterAddr))
    )

  /**
   * Sm/Ssstateen begin
   */

  //  SE0 bit 63
  private val accessStateen_EX_VI = (
    hstateen0.SE0.asBool +: Seq(hstateen1, hstateen2, hstateen3).map(_.SE.asBool)
    ).zipWithIndex.map{case(se, i) => {
    val csrIsSstateen = addr === (CSRs.sstateen0 + i).U
    csrIsSstateen && privState.isVirtual && !se
  }}.reduce(_ || _)

  // ENVCFG bit 62
  private val csrIsSenvcfg = addr === CSRs.senvcfg.U
  private val accessEnvcfg_EX_VI = csrIsSenvcfg && privState.isVirtual && !hstateen0.ENVCFG.asBool

  // CSRIND bit 60 indirect reg (Sscsrind extensions), this is not implemented
  // csr addr S: [0x150, 0x157]
  private val csrIsSi = Ireg.isInSCsrInd(addr)
  private val accessIND_EX_VI = csrIsSi && privState.isVirtual && !hstateen0.CSRIND.asBool

  // AIA bit 59
  private val ssAiaSaddr = addr === CSRs.stopi.U
  private val accessAIA_EX_VI = ssAiaSaddr && privState.isVirtual && !hstateen0.AIA.asBool

  // IMSIC bit 58 (Ssaia extension)
  private val csrIsStopei = addr === CSRs.stopei.U
  private val accessTopie_EX_VI = csrIsStopei && privState.isVirtual && !hstateen0.IMSIC.asBool

  // CONTEXT bit 57 context reg (Sdtrig extensions)
  private val csrIsScontext = addr === CSRs.scontext.U
  private val accessContext_EX_VI = csrIsScontext && privState.isVirtual && !hstateen0.CONTEXT.asBool

  // P1P13 bit 56, Read-only 0

  // Custom bit 0
  // [0x5c0, 0x5ff], [0x9c0, 0x9ff], [0xdc0, 0xdff]
  private val csrIsSCustom   = (addr(11, 10) =/= "b00".U) && (addr(9, 8) === "b01".U) && (addr(7, 6) === "b11".U)
  // [0x800, 0x8ff], [0xcc0, 0xcff]
  private val csrIsUCustom   = (addr(11, 8) === "b1000".U) || (addr(11, 6) === "b110011".U)
  private val accessCustom_EX_VI = (csrIsSCustom || csrIsUCustom) && privState.isVirtual && !hstateen0.C.asBool ||
    csrIsUCustom && privState.isModeVU && hstateen0.C.asBool && !sstateen0.C.asBool

  private val xstateControlAccess_EX_VI = accessStateen_EX_VI || accessEnvcfg_EX_VI || accessIND_EX_VI || accessAIA_EX_VI ||
    accessTopie_EX_VI || accessContext_EX_VI || accessCustom_EX_VI

  io.out.virtualLevelPermit_EX_II := rwVStopei_EX_II
  io.out.virtualLevelPermit_EX_VI := rwSatp_EX_VI || rwStopei_EX_VI || rwSip_Sie_EX_VI || rwStimecmp_EX_VI || accessHPM_EX_VI || xstateControlAccess_EX_VI
}

class IndirectCSRPermitModule extends Module {
  val io = IO(new Bundle() {
    val in = Input(new Bundle {
      val csrAccess = new csrAccessIO
      val privState = new PrivState
      val aia = new aiaIO
      val xstateen = new xstateenIO
    })
    val out = Output(new Bundle {
      val indirectCSR_EX_II = Bool()
      val indirectCSR_EX_VI = Bool()
    })
  })

  private val (addr, privState) = (
    io.in.csrAccess.addr,
    io.in.privState,
  )

  private val (miselect, siselect, vsiselect) = (
    io.in.aia.miselect,
    io.in.aia.siselect,
    io.in.aia.vsiselect,
  )

  private val (mstateen0, hstateen0) = (
    io.in.xstateen.mstateen0,
    io.in.xstateen.hstateen0,
  )

  private val mvienSEIE = io.in.aia.mvienSEIE

  private val rwMireg_EX_II = (
      Iselect.isInAIA(miselect) && Iselect.isOdd(miselect) ||
      Iselect.isInOthers(miselect)
    ) && addr === CSRs.mireg.U

  private val rwMireg2_6_EX_II = Ireg.isInMireg2_6(addr)

  private val rwSireg_EX_II = (
      !privState.isVirtual && (
        Iselect.isInAIA(siselect) && Iselect.isOdd(siselect) ||
        Iselect.isInOthers(siselect)
      ) ||
      privState.isModeHS && (
        mvienSEIE && Iselect.isInImsic(siselect) ||
        !mstateen0.AIA.asBool && Iselect.isInAIA(siselect) ||
        !mstateen0.IMSIC.asBool && Iselect.isInImsic(siselect)
      ) ||
      privState.isVirtual && (
        Iselect.isInOthers(vsiselect) ||
        !mstateen0.AIA.asBool && Iselect.isInAIA(vsiselect) ||
        !mstateen0.IMSIC.asBool && Iselect.isInImsic(vsiselect)
      )
    ) && addr === CSRs.sireg.U

  private val rwSireg_EX_VI = privState.isVirtual && (Iselect.isInAIA(vsiselect) || Iselect.isInImsic(vsiselect) && !hstateen0.IMSIC.asBool) && addr === CSRs.sireg.U

  private val rwSireg2_6_EX_VI = privState.isVirtual && (Iselect.isInAIA(vsiselect) || Iselect.isInImsic(vsiselect)) && Ireg.isInSireg2_6(addr)

  private val rwSireg2_6_EX_II = Ireg.isInSireg2_6(addr) && !rwSireg2_6_EX_VI

  private val rwVSireg_EX_II = (
      !Iselect.isInImsic(vsiselect) ||
      !privState.isModeM && !mstateen0.IMSIC.asBool
    ) && addr === CSRs.vsireg.U

  private val rwVSireg2_6_EX_II = Ireg.isInVSireg2_6(addr)

  io.out.indirectCSR_EX_II := rwMireg_EX_II || rwMireg2_6_EX_II || rwSireg_EX_II || rwSireg2_6_EX_II || rwVSireg_EX_II || rwVSireg2_6_EX_II
  io.out.indirectCSR_EX_VI := rwSireg_EX_VI || rwSireg2_6_EX_VI
}

class csrAccessIO extends Bundle {
  val ren   = Bool()
  val wen   = Bool()
  val addr  = UInt(12.W)
}

class xRetIO extends Bundle {
  val mnret = Bool()
  val mret = Bool()
  val sret = Bool()
  val dret = Bool()
}

class statusIO extends Bundle {
  // Trap SRET
  val tsr = Bool()
  // Virtual Trap SRET
  val vtsr = Bool()
  // Trap Virtual Memory
  val tvm = Bool()
  // Virtual Trap Virtual Memory
  val vtvm = Bool()
  val vgein = UInt(6.W)
  val mstatusFSOff = Bool()
  val vsstatusFSOff = Bool()
  val mstatusVSOff = Bool()
  val vsstatusVSOff = Bool()
  val mstatusMSOff = Bool()
  val vsstatusMSOff = Bool()
}

class xcounterenIO extends Bundle {
  // Machine level counter enable, access PMC from the level less than M will trap EX_II
  val mcounteren = UInt(32.W)
  // Hypervisor level counter enable.
  // Accessing PMC from VS/VU level will trap EX_VI, if m[x]=1 && h[x]=0
  val hcounteren = UInt(32.W)
  // Supervisor level counter enable.
  // Accessing PMC from **HU level** will trap EX_II, if s[x]=0
  // Accessing PMC from **VU level** will trap EX_VI, if m[x]=1 && h[x]=1 && s[x]=0
  val scounteren = UInt(32.W)
}

class xenvcfgIO extends Bundle {
  // Machine environment configuration register.
  // Accessing stimecmp or vstimecmp from **Non-M level** will trap EX_II, if menvcfg.STCE=0
  val menvcfg = UInt(64.W)
  // Hypervisor environment configuration register.
  // Accessing vstimecmp from ** V level** will trap EX_VI, if menvcfg.STCE=1 && henvcfg.STCE=0
  val henvcfg = UInt(64.W)
}

class xstateenIO extends Bundle {
  // Sm/Ssstateen: to control state access
  val mstateen0 = new Mstateen0Bundle
  val mstateen1 = new MstateenNonZeroBundle
  val mstateen2 = new MstateenNonZeroBundle
  val mstateen3 = new MstateenNonZeroBundle
  val hstateen0 = new Hstateen0Bundle
  val hstateen1 = new HstateenNonZeroBundle
  val hstateen2 = new HstateenNonZeroBundle
  val hstateen3 = new HstateenNonZeroBundle
  val sstateen0 = new Sstateen0Bundle
}

class aiaIO extends Bundle {
  val miselect = UInt(64.W)
  val siselect = UInt(64.W)
  val vsiselect = UInt(64.W)
  val mvienSEIE = Bool()
  val hvictlVTI = Bool()
}

class CSRPermitIO extends Bundle {
  val in = Input(new Bundle {
    val csrAccess = new csrAccessIO
    val privState = new PrivState
    val debugMode = Bool()
    val xRet = new xRetIO
    val status = new statusIO
    val xcounteren = new xcounterenIO
    val xenvcfg = new xenvcfgIO
    val xstateen = new xstateenIO
    val aia = new aiaIO
  })

  val out = Output(new Bundle {
    val hasLegalWen   = Bool()
    val hasLegalMNret = Bool()
    val hasLegalMret  = Bool()
    val hasLegalSret  = Bool()
    val hasLegalDret  = Bool()
    val hasLegalWriteFcsr = Bool()
    val hasLegalWriteVcsr = Bool()
    val hasLegalWriteMcsr = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
