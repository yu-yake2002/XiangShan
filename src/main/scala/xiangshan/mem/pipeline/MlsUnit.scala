package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.matrix.Bundles.{AmuLsuIO, AmuCtrlIO}
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.ExceptionNO._
import xiangshan.MldstOpType
import xiangshan.MldstOpType
import xiangshan.MldstOpType

class MlsToMlsqReplayIO(implicit p: Parameters) extends XSBundle
  with HasDCacheParameters
  with HasTlbConst
{
  // mshr refill index
  val mshr_id         = UInt(log2Up(cfg.nMissEntries).W)
  // get full data from store queue and sbuffer
  val full_fwd        = Bool()
  // replay carry
  val rep_carry       = new ReplayCarry(nWays)
  // data in last beat
  val last_beat       = Bool()
  // replay cause
  val cause           = Vec(MlsReplayCauses.allCauses, Bool())
  // performance debug information
  val debug           = new PerfDebugInfo
  // tlb hint
  val tlb_id          = UInt(log2Up(loadfiltersize).W)
  val tlb_full        = Bool()

  def tlb_miss = cause(MlsReplayCauses.C_TM)
  def need_rep      = cause.asUInt.orR
}

class MlsToMlsqIO(implicit p: Parameters) extends XSBundle {
  val lsin            = DecoupledIO(new MlsqWriteBundle)
}

class MlsUnit(implicit p: Parameters) extends XSModule
  with HasLoadHelper
  // with HasPerfEvents
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasVLSUParameters
  with SdtrigExt
{
  val io = IO(new Bundle() {
    // control
    val redirect      = Flipped(ValidIO(new Redirect))
    val csrCtrl       = Flipped(new CustomCSRCtrlIO)

    // flow in from backend
    val lsin          = Flipped(Decoupled(new MemExuInput(isMatrix = true)))
    // flow out to backend
    val lsout         = DecoupledIO(new MemExuOutput(isMatrix = true))
    
    val lsq           = new MlsToMlsqIO
    
    // queue-based replay
    val replay          = Flipped(Decoupled(new MlsPipelineBundle))
    val mlsq_rep_full   = Input(Bool())

    // data path
    val tlb           = new TlbRequestIO(2)
    val pmp           = Flipped(new PMPRespBundle()) // arrive same to tlb now
    val tlb_hint      = Flipped(new TlbHintReq)

    // TODO: Are they necessary here?
    // rs feedback
    val feedback_fast = ValidIO(new RSFeedback) // stage 2
    val feedback_slow = ValidIO(new RSFeedback) // stage 3

    // perf
    val debug_ls         = Output(new DebugLsInfoBundle)
    val lsTopdownInfo    = Output(new LsTopdownInfo)
  })

  val s1_ready, s2_ready, s3_ready, sx_can_go = WireInit(false.B)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  // generate addr, use addr to query DTLB
  val s0_valid         = Wire(Bool())
  val s0_kill          = Wire(Bool())
  val s0_vaddr         = Wire(UInt(VAddrBits.W))
  val s0_uop           = Wire(new DynInst)
  val s0_has_rob_entry = Wire(Bool())
  val s0_try_l2l       = Wire(Bool())
  val s0_rep_carry     = Wire(new ReplayCarry(nWays))
  val s0_isFirstIssue  = Wire(Bool())
  val s0_ld_rep        = Wire(Bool())
  val s0_sched_idx     = Wire(UInt())
  val s0_can_go        = s1_ready
  val s0_fire          = s0_valid && s0_can_go
  val s0_out           = Wire(new MlsqWriteBundle)

  // load flow select/gen
  // src0: load replayed by LSQ (io.ldu_io.replay)
  // src1: int read / software prefetch first issue from RS (io.lsin)
  // priority: high to low
  val s0_ld_flow             = MldstOpType.isLoad(s0_uop.fuOpType)
  val s0_rep_stall           = io.lsin.valid && isAfter(io.replay.bits.uop.robIdx, io.lsin.bits.uop.robIdx)
  private val SRC_NUM = 2
  private val Seq(
    lsq_rep_idx, int_iss_idx
  ) = (0 until SRC_NUM).toSeq
  // load/store flow source valid
  val s0_src_valid_vec = WireInit(VecInit(Seq(
    io.replay.valid && !io.replay.bits.forward_tlDchannel && !s0_rep_stall,
    io.lsin.valid, // int flow first issue or software prefetch
  )))
  // load/store flow source ready
  val s0_src_ready_vec = Wire(Vec(SRC_NUM, Bool()))
  s0_src_ready_vec(0) := true.B
  for(i <- 1 until SRC_NUM){
    s0_src_ready_vec(i) := !s0_src_valid_vec.take(i).reduce(_ || _)
  }
  // load/store flow source select (OH)
  val s0_src_select_vec = WireInit(VecInit((0 until SRC_NUM).map{i => s0_src_valid_vec(i) && s0_src_ready_vec(i)}))

  s0_valid := s0_src_valid_vec.reduce(_ || _) && !s0_kill

  // which is S0's out is ready and dcache is ready
  s0_kill := s0_out.uop.robIdx.needFlush(io.redirect)

  // query DTLB
  io.tlb.req.valid                   := s0_valid
  io.tlb.req.bits.cmd                := Mux(s0_ld_flow, TlbCmd.read, TlbCmd.write)
  io.tlb.req.bits.vaddr              := s0_vaddr
  io.tlb.req.bits.fullva             := s0_vaddr // TODO: check me!
  io.tlb.req.bits.checkfullva        := false.B  // TODO: check me!
  io.tlb.req.bits.hyperinst          := false.B  // TODO: Maybe we need hyperinst support here in the future
  io.tlb.req.bits.hlvx               := false.B
  io.tlb.req.bits.isPrefetch         := false.B
  io.tlb.req.bits.pmp_addr           := DontCare
  io.tlb.req.bits.size               := LSUOpType.size(s0_uop.fuOpType)
  io.tlb.req.bits.kill               := s0_kill
  io.tlb.req.bits.memidx.is_ld       := s0_ld_flow
  io.tlb.req.bits.memidx.is_st       := !s0_ld_flow
  io.tlb.req.bits.memidx.idx         := s0_uop.lqIdx.value
  io.tlb.req.bits.debug.robIdx       := s0_uop.robIdx
  io.tlb.req.bits.no_translate       := false.B
  io.tlb.req.bits.debug.pc           := s0_uop.pc
  io.tlb.req.bits.debug.isFirstIssue := s0_isFirstIssue

  // load flow priority mux
  def fromNullSource() = {
    s0_vaddr         := 0.U
    s0_uop           := 0.U.asTypeOf(new DynInst)
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_isFirstIssue  := false.B
    s0_ld_rep        := false.B
    s0_sched_idx     := 0.U
  }

  def fromNormalReplaySource(src: LsPipelineBundle) = {
    s0_vaddr         := src.vaddr
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rep_carry     := src.replayCarry
    s0_isFirstIssue  := false.B
    s0_ld_rep        := true.B
    s0_sched_idx     := src.schedIndex
  }

  def fromIntIssueSource(src: MemExuInput) = {
    s0_vaddr         := src.src(0)
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_isFirstIssue  := true.B
    s0_ld_rep        := false.B
    s0_sched_idx     := 0.U
  }

  // set default
  s0_uop := DontCare
  when (s0_src_select_vec(lsq_rep_idx)) {
    fromNormalReplaySource(io.replay.bits)
  }.elsewhen (s0_src_select_vec(int_iss_idx)) {
    fromIntIssueSource(io.lsin.bits)
  }.otherwise {
    fromNullSource()
  }

  // TODO: address align check
  val s0_addr_aligned = true.B

  s0_out               := DontCare
  s0_out.vaddr         := s0_vaddr
  s0_out.uop           := s0_uop
  s0_out.isFirstIssue  := s0_isFirstIssue
  s0_out.hasROBEntry   := s0_has_rob_entry
  s0_out.isLoadReplay  := s0_ld_rep
  s0_out.stride        := io.lsin.bits.src(1)
  s0_out.mtile0        := io.lsin.bits.src(2)
  s0_out.mtile1        := io.lsin.bits.src(3)
  s0_out.uop.exceptionVec(loadAddrMisaligned)  := !s0_addr_aligned && s0_ld_flow
  s0_out.uop.exceptionVec(storeAddrMisaligned) := !s0_addr_aligned && !s0_ld_flow
  s0_out.forward_tlDchannel := false.B
  when(io.tlb.req.valid && s0_isFirstIssue) {
    s0_out.uop.debugInfo.tlbFirstReqTime := GTimer()
  }.otherwise{
    s0_out.uop.debugInfo.tlbFirstReqTime := s0_uop.debugInfo.tlbFirstReqTime
  }
  s0_out.schedIndex     := s0_sched_idx

  // load flow source ready
  // cache missed load has highest priority
  // always accept cache missed load flow from load replay queue
  io.replay.ready := (s0_can_go && (s0_src_ready_vec(lsq_rep_idx) && !s0_rep_stall))

  io.lsin.ready := s0_can_go && s0_src_ready_vec(int_iss_idx)

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  // TLB resp
  val s1_valid      = RegInit(false.B)
  val s1_in         = Wire(new MlsqWriteBundle)
  val s1_out        = Wire(new MlsqWriteBundle)
  val s1_kill       = Wire(Bool())
  val s1_can_go     = s2_ready
  val s1_fire       = s1_valid && !s1_kill && s1_can_go
  val s1_ld_flow    = RegNext(s0_ld_flow)
  val s1_isLastElem = RegEnable(s0_out.isLastElem, false.B, s0_fire)

  s1_ready := !s1_valid || s1_kill || s2_ready
  when (s0_fire) { s1_valid := true.B }
  .elsewhen (s1_fire) { s1_valid := false.B }
  .elsewhen (s1_kill) { s1_valid := false.B }
  s1_in   := RegEnable(s0_out, s0_fire)

  val s1_late_kill        = false.B
  val s1_vaddr_hi         = Wire(UInt())
  val s1_vaddr_lo         = Wire(UInt())
  val s1_vaddr            = Wire(UInt())
  val s1_paddr_dup_lsu    = Wire(UInt())
  val s1_ld_exception     = ExceptionNO.selectByFu(s1_out.uop.exceptionVec, LduCfg).asUInt.orR   // af & pf exception were modified below.
  val s1_st_exception     = ExceptionNO.selectByFu(s1_out.uop.exceptionVec, StaCfg).asUInt.orR   // af & pf exception were modified below.
  val s1_exception        = (s1_ld_flow && s1_ld_exception) || (!s1_ld_flow && s1_st_exception)
  val s1_tlb_miss         = io.tlb.resp.bits.miss
  val s1_tlb_memidx       = io.tlb.resp.bits.memidx

  // mmio cbo decoder
  val s1_mmio_cbo  = (s1_in.uop.fuOpType === LSUOpType.cbo_clean ||
                      s1_in.uop.fuOpType === LSUOpType.cbo_flush ||
                      s1_in.uop.fuOpType === LSUOpType.cbo_inval) && !s1_ld_flow
  val s1_mmio = s1_mmio_cbo

  s1_vaddr_hi         := s1_in.vaddr(VAddrBits - 1, 6)
  s1_vaddr_lo         := s1_in.vaddr(5, 0)
  s1_vaddr            := Cat(s1_vaddr_hi, s1_vaddr_lo)
  s1_paddr_dup_lsu    := io.tlb.resp.bits.paddr(0)

  when (s1_tlb_memidx.is_ld && io.tlb.resp.valid && !s1_tlb_miss &&
        s1_tlb_memidx.idx === s1_in.uop.lqIdx.value && s1_ld_flow) {
    // printf("Load idx = %d\n", s1_tlb_memidx.idx)
    s1_out.uop.debugInfo.tlbRespTime := GTimer()
  } .elsewhen(s1_tlb_memidx.is_st && io.tlb.resp.valid && !s1_tlb_miss &&
              s1_tlb_memidx.idx === s1_out.uop.sqIdx.value && !s1_ld_flow) {
    // printf("Store idx = %d\n", s1_tlb_memidx.idx)
    s1_out.uop.debugInfo.tlbRespTime := GTimer()
  }

  io.tlb.req_kill   := s1_kill
  io.tlb.resp.ready := true.B

  s1_out                   := s1_in
  s1_out.vaddr             := s1_vaddr
  s1_out.paddr             := s1_paddr_dup_lsu
  s1_out.tlbMiss           := s1_tlb_miss
  s1_out.ptwBack           := io.tlb.resp.bits.ptwBack
  s1_out.rep_info.debug    := s1_in.uop.debugInfo
  s1_out.lateKill          := s1_late_kill

  // TODO: store trigger
  // val storeTrigger = Module(new MemTrigger(MemType.STORE))

  when (s1_ld_flow) {
    when (!s1_late_kill) {
      // current ori test will cause the case of ldest == 0, below will be modifeid in the future.
      // af & pf exception were modified
      s1_out.uop.exceptionVec(loadPageFault)       := io.tlb.resp.bits.excp(0).pf.ld
      s1_out.uop.exceptionVec(loadGuestPageFault)  := io.tlb.resp.bits.excp(0).gpf.ld
      s1_out.uop.exceptionVec(loadAccessFault)     := io.tlb.resp.bits.excp(0).af.ld
    } .otherwise {
      s1_out.uop.exceptionVec(loadAddrMisaligned)  := false.B
      s1_out.uop.exceptionVec(loadAccessFault)     := s1_late_kill
    }
  } .otherwise {
    s1_out.uop.exceptionVec(storePageFault)        := io.tlb.resp.bits.excp(0).pf.st
    s1_out.uop.exceptionVec(storeGuestPageFault)   := io.tlb.resp.bits.excp(0).gpf.st
    s1_out.uop.exceptionVec(storeAccessFault)      := io.tlb.resp.bits.excp(0).af.st
  }

  // TODO: load trigger
  // val loadTrigger = Module(new MemTrigger(MemType.LOAD))

  s1_kill := s1_late_kill ||
             s1_in.uop.robIdx.needFlush(io.redirect) ||
             RegEnable(s0_kill, false.B, io.lsin.valid || io.replay.valid)

  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val s1_sqIdx_mask = RegNext(UIntToMask(s0_out.uop.sqIdx.value, StoreQueueSize))

  // load debug
  XSDebug(s1_valid && s1_ld_flow,
    p"S1: pc ${Hexadecimal(s1_out.uop.pc)}, lId ${Hexadecimal(s1_out.uop.lqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(s1_out.paddr)}, mmio ${s1_out.mmio}\n")

  // store debug
  XSDebug(s1_valid && !s1_ld_flow,
    p"S1: pc ${Hexadecimal(s1_out.uop.pc)}, lId ${Hexadecimal(s1_out.uop.sqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(s1_out.paddr)}, mmio ${s1_out.mmio}\n")

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  // s2: DCache resp
  val s2_valid  = RegInit(false.B)
  val s2_in     = Wire(new MlsqWriteBundle)
  val s2_out    = Wire(new MlsqWriteBundle)
  val s2_kill   = Wire(Bool())
  val s2_can_go = s3_ready
  val s2_fire   = s2_valid && !s2_kill && s2_can_go
  val s2_paddr  = RegEnable(s1_paddr_dup_lsu, s1_fire)

  s2_kill := s2_in.uop.robIdx.needFlush(io.redirect)
  s2_ready := !s2_valid || s2_kill || s3_ready
  when (s1_fire) {
    s2_valid := true.B
  }.elsewhen (s2_fire) {
    s2_valid := false.B
  }.elsewhen (s2_kill) {
    s2_valid := false.B
  }
  s2_in := RegEnable(s1_out, s1_fire)

  val s2_pmp = WireInit(io.pmp)
  val s2_ld_flow  = RegEnable(s1_ld_flow, s1_fire)

  // exception that may cause load addr to be invalid / illegal
  // if such exception happen, that inst and its exception info
  // will be force writebacked to rob
  val s2_exception_vec = WireInit(s2_in.uop.exceptionVec)
  when (s2_ld_flow) {
    when (!s2_in.lateKill) {
      s2_exception_vec(loadAccessFault) := s2_in.uop.exceptionVec(loadAccessFault) || s2_pmp.ld
      // soft prefetch will not trigger any exception (but ecc error interrupt may be triggered)
      when (s2_in.tlbMiss) {
        s2_exception_vec := 0.U.asTypeOf(s2_exception_vec.cloneType)
      }
    }
  } .otherwise {
    s2_exception_vec(storeAccessFault) := s2_in.uop.exceptionVec(storeAccessFault) || s2_pmp.st
    when (s2_in.tlbMiss) {
      s2_exception_vec := 0.U.asTypeOf(s2_exception_vec.cloneType)
    }
  }
  val s2_ld_exception = ExceptionNO.selectByFu(s2_exception_vec, LduCfg).asUInt.orR && s2_ld_flow
  val s2_st_exception = ExceptionNO.selectByFu(s2_exception_vec, StaCfg).asUInt.orR && !s2_ld_flow
  val s2_exception    = s2_ld_exception || s2_st_exception
  val s2_safe_wakeup = !s2_out.rep_info.need_rep && !s2_exception
  val s2_safe_writeback = s2_exception || s2_safe_wakeup

  // writeback access fault caused by ecc error / bus error
  // * ecc data error is slow to generate, so we will not use it until load stage 3
  // * in load stage 3, an extra signal io.load_error will be used to
  val s2_actually_mmio = s2_pmp.mmio
  val s2_ld_mmio       =  s2_actually_mmio &&
                         !s2_exception &&
                         !s2_in.tlbMiss &&
                         s2_ld_flow
  val s2_st_mmio       =  (RegNext(s1_mmio) || s2_pmp.mmio) &&
                         !s2_exception &&
                         !s2_in.tlbMiss &&
                         !s2_ld_flow
  val s2_st_atomic     =  (RegNext(s1_mmio) || s2_pmp.atomic) &&
                         !s2_exception &&
                         !s2_in.tlbMiss &&
                         !s2_ld_flow

  val s2_tlb_miss      = s2_in.tlbMiss

  val s2_troublem        = !s2_exception &&
                           !s2_ld_mmio &&
                           !s2_in.lateKill

  // need allocate new entry
  val s2_can_query = !s2_tlb_miss && s2_troublem

  //
  s2_out                  := s2_in
  s2_out.data             := 0.U // data will be generated in load s3
  s2_out.uop.fpWen        := s2_in.uop.fpWen && !s2_exception && s2_ld_flow
  s2_out.mmio             := s2_ld_mmio || s2_st_mmio
  s2_out.atomic           := s2_st_atomic
  s2_out.uop.flushPipe    := false.B
  s2_out.uop.exceptionVec := s2_exception_vec
  s2_out.miss             := false.B
  s2_out.feedbacked       := io.feedback_fast.valid && !io.feedback_fast.bits.hit

  // Generate replay signal caused by tlb miss
  s2_out.rep_info.tlb_miss        := s2_tlb_miss && s2_troublem
  s2_out.rep_info.rep_carry       := 0.U.asTypeOf(s2_out.rep_info.rep_carry.cloneType)
  s2_out.rep_info.last_beat       := s2_in.paddr(log2Up(refillBytes))
  s2_out.rep_info.debug           := s2_in.uop.debugInfo
  s2_out.rep_info.tlb_id          := io.tlb_hint.id
  s2_out.rep_info.tlb_full        := io.tlb_hint.full

  // to be removed
  val s2_need_fb = !s2_in.isLoadReplay &&      // already feedbacked
                   io.mlsq_rep_full &&         // MlsQueueReplay is full
                   s2_out.rep_info.need_rep && // need replay
                   !s2_exception               // no exception is triggered
  io.feedback_fast.valid                 := s2_valid && s2_need_fb
  io.feedback_fast.bits.hit              := Mux(s2_ld_flow, false.B, !s2_tlb_miss)
  io.feedback_fast.bits.flushState       := s2_in.ptwBack
  io.feedback_fast.bits.robIdx           := s2_in.uop.robIdx
  io.feedback_fast.bits.sourceType       := Mux(s2_ld_flow, RSFeedbackType.lrqFull, RSFeedbackType.tlbMiss)
  io.feedback_fast.bits.dataInvalidSqIdx := DontCare
  io.feedback_fast.bits.lqIdx            := s2_in.uop.lqIdx
  io.feedback_fast.bits.sqIdx            := s2_in.uop.sqIdx
  io.feedback_fast.bits.mlsqIdx          := s2_in.uop.mlsqIdx

  val s1_ld_left_fire = s1_valid && !s1_kill && s2_ready && s1_ld_flow
  val s2_ld_valid_dup = RegInit(0.U(6.W))
  s2_ld_valid_dup := 0x0.U(6.W)
  when (s1_ld_left_fire && s1_ld_flow) { s2_ld_valid_dup := 0x3f.U(6.W) }
  when (s1_kill || !s1_ld_flow) { s2_ld_valid_dup := 0x0.U(6.W) }
  assert(RegNext((s2_valid === s2_ld_valid_dup(0)) || RegNext(!s1_ld_flow)))

  // Pipeline
  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  // writeback and update load queue
  val s3_valid        = RegNext(s2_valid && !s2_out.uop.robIdx.needFlush(io.redirect))
  val s3_in           = RegEnable(s2_out, s2_fire)
  val s3_out          = Wire(Valid(new MemExuOutput(isMatrix = true)))
  val s3_ld_valid_dup = RegEnable(s2_ld_valid_dup, s2_fire)
  val s3_ld_flow      = RegNext(s2_ld_flow)
  val s3_troublem     = RegNext(s2_troublem)
  val s3_kill         = s3_in.uop.robIdx.needFlush(io.redirect)
  s3_ready := !s3_valid || s3_kill || io.lsout.ready

  io.lsq.lsin.valid := s3_valid && !s3_in.feedbacked && !s3_in.lateKill
  io.lsq.lsin.bits := s3_in
  io.lsq.lsin.bits.miss := s3_in.miss

  /* <------- DANGEROUS: Don't change sequence here ! -------> */
  io.lsq.lsin.bits.data_wen_dup := s3_ld_valid_dup.asBools
  io.lsq.lsin.bits.missDbUpdated := RegNext(s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss && !s2_in.missDbUpdated)
  io.lsq.lsin.bits.updateAddrValid := true.B
  io.lsq.lsin.bits.dcacheRequireReplay  := false.B

  val s3_vp_match_fail = s3_troublem
  val s3_ldld_rep_inst = false.B
  val s3_safe_writeback = RegEnable(s2_safe_writeback, s2_fire)
  val s3_rep_info = WireInit(s3_in.rep_info)
  val s3_rep_frm_fetch = s3_vp_match_fail
  val s3_flushPipe = s3_ldld_rep_inst
  val s3_sel_rep_cause = PriorityEncoderOH(s3_rep_info.cause.asUInt)
  val s3_force_rep     = s3_sel_rep_cause(MlsReplayCauses.C_TM) &&
                         !s3_in.uop.exceptionVec(loadAddrMisaligned) &&
                         s3_troublem

  val s3_ld_exception = ExceptionNO.selectByFu(s3_in.uop.exceptionVec, LduCfg).asUInt.orR && s3_ld_flow
  val s3_st_exception = ExceptionNO.selectByFu(s3_in.uop.exceptionVec, StaCfg).asUInt.orR && !s3_ld_flow
  val s3_exception    = s3_ld_exception || s3_st_exception
  when ((s3_ld_exception || s3_rep_frm_fetch) && !s3_force_rep) {
    io.lsq.lsin.bits.rep_info.cause := 0.U.asTypeOf(s3_rep_info.cause.cloneType)
  } .otherwise {
    io.lsq.lsin.bits.rep_info.cause := VecInit(s3_sel_rep_cause.asBools)
  }

  val amuCtrl = Wire(new AmuLsuIO)
  amuCtrl.ls := MldstOpType.isStore(s3_in.uop.fuOpType)
  amuCtrl.ms := s3_in.uop.instr(3, 0)
  amuCtrl.widths    := Mux1H(Seq(
    MldstOpType.isFp8(s3_in.uop.fuOpType)  -> 3.U,
    MldstOpType.isFp16(s3_in.uop.fuOpType) -> 4.U,
    MldstOpType.isFp32(s3_in.uop.fuOpType) -> 5.U,
  ))
  amuCtrl.baseAddr  := s3_in.paddr
  amuCtrl.stride    := s3_in.stride
  amuCtrl.transpose := MldstOpType.isTransposed(s3_in.uop.fuOpType)
  amuCtrl.row       := s3_in.mtile0
  amuCtrl.column    := s3_in.mtile1

  // Int flow, if hit, will be writebacked at s3
  // s3_out.valid                 := s3_valid &&
  //                               (!s3_ld_flow && !s3_in.feedbacked || !io.lsq.lsin.bits.rep_info.need_rep) &&
  //                               !s3_in.mmio
  s3_out.valid                 := s3_valid && s3_safe_writeback
  s3_out.bits.uop              := s3_in.uop
  s3_out.bits.uop.exceptionVec(loadAccessFault) := s3_in.uop.exceptionVec(loadAccessFault) && s3_ld_flow
  s3_out.bits.uop.replayInst   := false.B
  s3_out.bits.data             := s3_in.data
  s3_out.bits.amuCtrl.get.op   := AmuCtrlIO.mlsOp()
  s3_out.bits.amuCtrl.get.data := amuCtrl.asUInt
  s3_out.bits.debug.isMMIO     := s3_in.mmio
  s3_out.bits.debug.isNC       := s3_in.nc
  s3_out.bits.debug.isPerfCnt  := false.B
  s3_out.bits.debug.paddr      := s3_in.paddr
  s3_out.bits.debug.vaddr      := s3_in.vaddr
  s3_out.bits.isFromLoadUnit   := MldstOpType.isLoad(s3_in.uop.fuOpType)

  when (s3_force_rep) {
    s3_out.bits.uop.exceptionVec := 0.U.asTypeOf(s3_in.uop.exceptionVec.cloneType)
  }

  /* <------- DANGEROUS: Don't change sequence here ! -------> */
  io.lsq.lsin.bits.uop := s3_out.bits.uop

  val s3_revoke = s3_exception || io.lsq.lsin.bits.rep_info.need_rep

  val s3_fb_no_waiting = !s3_in.isLoadReplay && !s3_in.feedbacked

  //
  io.feedback_slow.valid                 := s3_valid && !s3_in.uop.robIdx.needFlush(io.redirect) && s3_fb_no_waiting && s3_ld_flow
  io.feedback_slow.bits.hit              := !io.lsq.lsin.bits.rep_info.need_rep || io.lsq.lsin.ready
  io.feedback_slow.bits.flushState       := s3_in.ptwBack
  io.feedback_slow.bits.robIdx           := s3_in.uop.robIdx
  io.feedback_slow.bits.sourceType       := RSFeedbackType.lrqFull
  io.feedback_slow.bits.dataInvalidSqIdx := DontCare
  io.feedback_slow.bits.lqIdx            := s3_in.uop.lqIdx
  io.feedback_slow.bits.sqIdx            := s3_in.uop.sqIdx
  io.feedback_slow.bits.mlsqIdx          := s3_in.uop.mlsqIdx

  io.lsout.bits      := s3_out.bits
  io.lsout.valid     := s3_out.valid && !s3_out.bits.uop.robIdx.needFlush(io.redirect)

  // hybrid unit writeback to rob
  // delay params
  val SelectGroupSize   = RollbackGroupSize
  val lgSelectGroupSize = log2Ceil(SelectGroupSize)
  val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1
  val TotalDelayCycles  = TotalSelectCycles - 2

  // FIXME: please move this part to LoadQueueReplay
  io.debug_ls := DontCare

  // Topdown
  io.lsTopdownInfo.s1.robIdx          := s1_in.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid     := s1_valid && s1_in.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits      := s1_vaddr
  io.lsTopdownInfo.s2.robIdx          := s2_in.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid     := s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits      := s2_in.paddr
  io.lsTopdownInfo.s2.first_real_miss := false.B
  io.lsTopdownInfo.s2.cache_miss_en   := s2_fire && s2_in.hasROBEntry && !s2_in.tlbMiss && !s2_in.missDbUpdated
}