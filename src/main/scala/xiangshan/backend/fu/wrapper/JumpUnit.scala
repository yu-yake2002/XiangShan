package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.{SignExt, ZeroExt}
import xiangshan.RedirectLevel
import xiangshan.backend.fu.{FuConfig, FuncUnit, JumpDataModule, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData


class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val jumpDataModule = Module(new JumpDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.data.src(0)
  private val pc = Mux(io.instrAddrTransType.get.shouldBeSext,
    SignExt(io.in.bits.data.pc.get, cfg.destDataBits),
    ZeroExt(io.in.bits.data.pc.get, cfg.destDataBits)
  )
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  private val isRVC = io.in.bits.ctrl.preDecode.get.isRVC

  jumpDataModule.io.src := src
  jumpDataModule.io.pc := pc
  jumpDataModule.io.imm := imm
  jumpDataModule.io.nextPcOffset := io.in.bits.data.nextPcOffset.get
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  val jmpTarget = io.in.bits.ctrl.predictInfo.get.target
  val predTaken = io.in.bits.ctrl.predictInfo.get.taken

  val redirect = io.out.bits.res.redirect.get.bits
  val redirectValid = io.out.bits.res.redirect.get.valid
  redirectValid := io.in.valid && !jumpDataModule.io.isAuipc
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.fullTarget := jumpDataModule.io.target
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := jumpDataModule.io.target
  redirect.cfiUpdate.pc := io.in.bits.data.pc.get
  redirect.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrData().dataWidth - 1, 0) =/= jmpTarget || !predTaken
  redirect.cfiUpdate.backendIAF := io.instrAddrTransType.get.checkAccessFault(jumpDataModule.io.target)
  redirect.cfiUpdate.backendIPF := io.instrAddrTransType.get.checkPageFault(jumpDataModule.io.target)
  redirect.cfiUpdate.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(jumpDataModule.io.target)
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := jumpDataModule.io.result
  connect0LatencyCtrlSingal
}
