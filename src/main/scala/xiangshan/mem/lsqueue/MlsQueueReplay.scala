package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.FuConfig.MlsCfg
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.ExceptionNO._
import math._

object MlsReplayCauses {
  // these causes have priority, lower coding has higher priority.
  // when load replay happens, load unit will select highest priority
  // from replay causes vector

  /*
   * Warning:
   * ************************************************************
   * * Don't change the priority. If the priority is changed,   *
   * * deadlock may occur. If you really need to change or      *
   * * add priority, please ensure that no deadlock will occur. *
   * ************************************************************
   *
   */
  // tlb miss check
  val C_TM  = 0
  // total causes
  val allCauses = 1
}

class MlsReplayInfo(implicit p: Parameters) extends XSBundle with HasVLSUParameters {
  val isvec = Bool()
  val isLastElem = Bool()
  val is128bit = Bool()
  val uop_unit_stride_fof = Bool()
  val usSecondInv = Bool()
  val elemIdx = UInt(elemIdxBits.W)
  val alignedType = UInt(alignTypeBits.W)
  val mbIndex = UInt(max(vlmBindexBits, vsmBindexBits).W)
  val elemIdxInsideVd = UInt(elemIdxBits.W)
  val reg_offset = UInt(vOffsetBits.W)
  val vecActive = Bool()
  val is_first_ele = Bool()
  val mask = UInt((VLEN/8).W)

  val stride = UInt(PAddrBits.W)
  val mtile0 = UInt(XLEN.W)
  val mtile1 = UInt(XLEN.W)
}

class MlsQueueReplay(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasTlbConst
{
  val io = IO(new Bundle {
    // control
    val redirect = Flipped(Valid(new Redirect))

    // from Mls unit s3
    val enq = Vec(backendParams.MlsCnt, Flipped(Decoupled(new MlsqWriteBundle)))

    // queue-based replay
    val replay = Vec(backendParams.MlsCnt, Decoupled(new MlsPipelineBundle))

    val mlsqFull = Output(Bool())
    val mlsWbPtr = Input(new MlsqPtr)
    val tlb_hint = Flipped(new TlbHintIO)
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))
  })

  println("MlsQueueReplay size: " + MlsQueueReplaySize)
  //  LoadQueueReplay field:
  //  +-----------+---------+-------+-------------+--------+
  //  | Allocated | MicroOp | VAddr |    Cause    |  Flags |
  //  +-----------+---------+-------+-------------+--------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  VAddr       : virtual address
  //  Cause       : replay cause
  //  Flags       : rar/raw queue allocate flags
  val allocated = RegInit(VecInit(List.fill(MlsQueueReplaySize)(false.B))) // The control signals need to explicitly indicate the initial value
  val scheduled = RegInit(VecInit(List.fill(MlsQueueReplaySize)(false.B)))
  val uop = Reg(Vec(MlsQueueReplaySize, new DynInst))
  val vecReplay = Reg(Vec(MlsQueueReplaySize, new MlsReplayInfo))
  val vaddrModule = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W),
    numEntries = MlsQueueReplaySize,
    numRead = MlsPipelineWidth,
    numWrite = MlsPipelineWidth,
    numWBank = MlsQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 0))
  vaddrModule.io := DontCare
  val debug_vaddr = RegInit(VecInit(List.fill(MlsQueueReplaySize)(0.U(VAddrBits.W))))
  val cause = RegInit(VecInit(List.fill(MlsQueueReplaySize)(0.U(MlsReplayCauses.allCauses.W))))
  val blocking = RegInit(VecInit(List.fill(MlsQueueReplaySize)(false.B)))
  
  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = MlsQueueReplaySize,
    allocWidth = MlsPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = true,
    moduleName = "MlsQueueReplay freelist"
  ))
  freeList.io := DontCare
  /**
   * used for re-select control
   */
  val tlbHintId = RegInit(VecInit(List.fill(MlsQueueReplaySize)(0.U((log2Up(loadfiltersize+1).W)))))
  val missDbUpdated = RegInit(VecInit(List.fill(MlsQueueReplaySize)(false.B)))
  //  LoadQueueReplay deallocate
  val freeMaskVec = Wire(Vec(MlsQueueReplaySize, Bool()))

  /**
   * Enqueue
   */
  val canEnqueue = io.enq.map(_.valid)
  val cancelEnq = io.enq.map(enq => enq.bits.uop.robIdx.needFlush(io.redirect))
  val needReplay = io.enq.map(enq => enq.bits.rep_info.need_rep)
  val hasExceptions = io.enq.map(enq => ExceptionNO.selectByFu(enq.bits.uop.exceptionVec, MlsCfg).asUInt.orR && !enq.bits.tlbMiss)
  val loadReplay = io.enq.map(enq => enq.bits.isLoadReplay)
  val needEnqueue = VecInit((0 until MlsPipelineWidth).map(w => {
    canEnqueue(w) && !cancelEnq(w) && needReplay(w) && !hasExceptions(w)
  }))
  val newEnqueue = Wire(Vec(MlsPipelineWidth, Bool()))
  val canFreeVec = VecInit((0 until MlsPipelineWidth).map(w => {
    canEnqueue(w) && loadReplay(w) && (!needReplay(w) || hasExceptions(w))
  }))

  // select MlsPipelineWidth valid index.
  val mlsqFull = freeList.io.empty
  val mlsqFreeNums = freeList.io.validCount

  // replay logic
  // release logic generation
  val storeAddrInSameCycleVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  val storeDataInSameCycleVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  val addrNotBlockVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  val dataNotBlockVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  val storeAddrValidVec = addrNotBlockVec.asUInt | storeAddrInSameCycleVec.asUInt
  val storeDataValidVec = dataNotBlockVec.asUInt | storeDataInSameCycleVec.asUInt

  for (i <- 0 until MlsQueueReplaySize) {
    // dequeue
    dataNotBlockVec(i) := true.B
    addrNotBlockVec(i) := true.B
    // store address execute
    storeAddrInSameCycleVec(i) := VecInit((0 until StorePipelineWidth).map(w => {false.B})).asUInt.orR
    // store data execute
    storeDataInSameCycleVec(i) := VecInit((0 until StorePipelineWidth).map(w => {false.B})).asUInt.orR
  }

  // store addr issue check
  val stAddrDeqVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  (0 until MlsQueueReplaySize).map(i => {
    stAddrDeqVec(i) := allocated(i) && storeAddrValidVec(i)
  })

  // store data issue check
  val stDataDeqVec = Wire(Vec(MlsQueueReplaySize, Bool()))
  (0 until MlsQueueReplaySize).map(i => {
    stDataDeqVec(i) := allocated(i) && storeDataValidVec(i)
  })

  // update blocking condition
  (0 until MlsQueueReplaySize).map(i => {
    // case C_TM
    when (cause(i)(MlsReplayCauses.C_TM)) {
      blocking(i) := Mux(io.tlb_hint.resp.valid &&
                     (io.tlb_hint.resp.bits.replay_all ||
                     io.tlb_hint.resp.bits.id === tlbHintId(i)), false.B, blocking(i))
    }
  })

  //  Replay is splitted into 3 stages
  require((MlsQueueReplaySize % MlsPipelineWidth) == 0)
  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until MlsQueueReplaySize / MlsPipelineWidth).map(i => { input(MlsPipelineWidth * i + rem) })).asUInt
  }

  def getRemSeq(input: Seq[Seq[Bool]])(rem: Int) = {
    (0 until MlsQueueReplaySize / MlsPipelineWidth).map(i => { input(MlsPipelineWidth * i + rem) })
  }

  // stage1: select 2 entries and read their vaddr
  val s0_oldestSel = Wire(Vec(MlsPipelineWidth, Valid(UInt(MlsQueueReplaySize.W))))
  val s1_can_go = Wire(Vec(MlsPipelineWidth, Bool()))
  val s1_oldestSel = Wire(Vec(MlsPipelineWidth, Valid(UInt(log2Up(MlsQueueReplaySize + 1).W))))
  val s2_can_go = Wire(Vec(MlsPipelineWidth, Bool()))
  val s2_oldestSel = Wire(Vec(MlsPipelineWidth, Valid(UInt(log2Up(MlsQueueReplaySize + 1).W))))

  // generate mask
  val needCancel = Wire(Vec(MlsQueueReplaySize, Bool()))
  // generate enq mask
  val enqIndexOH = Wire(Vec(MlsPipelineWidth, UInt(MlsQueueReplaySize.W)))
  val s0_loadEnqFireMask = newEnqueue.zip(enqIndexOH).map(x => Mux(x._1, x._2, 0.U))
  val s0_remLoadEnqFireVec = s0_loadEnqFireMask.map(x => VecInit((0 until MlsPipelineWidth).map(rem => getRemBits(x)(rem))))
  val s0_remEnqSelVec = Seq.tabulate(MlsPipelineWidth)(w => VecInit(s0_remLoadEnqFireVec.map(x => x(w))))

  // generate free mask
  val s0_loadFreeSelMask = GatedRegNext(freeMaskVec.asUInt)
  val s0_remFreeSelVec = VecInit(Seq.tabulate(MlsPipelineWidth)(rem => getRemBits(s0_loadFreeSelMask)(rem)))

  // l2 hint wakes up cache missed load
  // l2 will send GrantData in next 2/3 cycle, wake up the missed load early and sent them to load pipe, so them will hit the data in D channel or mshr in load S1
  val s0_loadHintWakeMask = VecInit((0 until MlsQueueReplaySize).map(i => {false.B})).asUInt
  // l2 will send 2 beats data in 2 cycles, so if data needed by this load is in first beat, select it this cycle, otherwise next cycle
  // when isKeyword = 1, s0_loadHintSelMask need overturn
  val s0_loadHintSelMask = s0_loadHintWakeMask
  val s0_remLoadHintSelMask = VecInit((0 until MlsPipelineWidth).map(rem => getRemBits(s0_loadHintSelMask)(rem)))
  val s0_remHintSelValidVec = VecInit((0 until MlsPipelineWidth).map(rem => ParallelORR(s0_remLoadHintSelMask(rem))))
  val s0_hintSelValid = ParallelORR(s0_loadHintSelMask)

  // wake up cache missed load
  (0 until MlsQueueReplaySize).foreach(i => {
    when(s0_loadHintWakeMask(i)) {
      blocking(i) := false.B
    }
  })

  // generate replay mask
  // replay select priority is given as follow
  // 1. hint wake up load
  // 2. higher priority load
  // 3. lower priority load
  val s0_loadHigherPriorityReplaySelMask = VecInit((0 until MlsQueueReplaySize).map(i => {false.B})).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadHigherPriorityReplaySelMask = VecInit((0 until MlsPipelineWidth).map(rem => getRemBits(s0_loadHigherPriorityReplaySelMask)(rem)))
  val s0_loadLowerPriorityReplaySelMask = VecInit((0 until MlsQueueReplaySize).map(i => {
    allocated(i) && !scheduled(i) && !blocking(i)
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadLowerPriorityReplaySelMask = VecInit((0 until MlsPipelineWidth).map(rem => getRemBits(s0_loadLowerPriorityReplaySelMask)(rem)))
  val s0_loadNormalReplaySelMask = s0_loadLowerPriorityReplaySelMask | s0_loadHintSelMask
  val s0_remNormalReplaySelVec = VecInit((0 until MlsPipelineWidth).map(rem => s0_remLoadLowerPriorityReplaySelMask(rem) | s0_remLoadHigherPriorityReplaySelMask(rem) | s0_remLoadHintSelMask(rem)))
  val s0_remPriorityReplaySelVec = VecInit((0 until MlsPipelineWidth).map(rem => {
        Mux(s0_remHintSelValidVec(rem), s0_remLoadHintSelMask(rem),
          Mux(ParallelORR(s0_remLoadHigherPriorityReplaySelMask(rem)), s0_remLoadHigherPriorityReplaySelMask(rem), s0_remLoadLowerPriorityReplaySelMask(rem)))
      }))
  /******************************************************************************************************
   * WARNING: Make sure that OldestSelectStride must less than or equal stages of load pipeline.        *
   ******************************************************************************************************
   */
  val OldestSelectStride = 4
  val oldestPtrExt = (0 until OldestSelectStride).map(i => io.mlsWbPtr + i.U)
  val s0_oldestMatchMaskVec = (0 until MlsQueueReplaySize).map(i => (0 until OldestSelectStride).map(j => s0_loadNormalReplaySelMask(i) && uop(i).mlsqIdx === oldestPtrExt(j)))
  val s0_remOldsetMatchMaskVec = (0 until MlsPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.take(1)))(rem))
  val s0_remOlderMatchMaskVec = (0 until MlsPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.drop(1)))(rem))
  val s0_remOldestSelVec = VecInit(Seq.tabulate(MlsPipelineWidth)(rem => {
    VecInit((0 until MlsQueueReplaySize / MlsPipelineWidth).map(i => {
      Mux(ParallelORR(s0_remOldsetMatchMaskVec(rem).map(_(0))), s0_remOldsetMatchMaskVec(rem)(i)(0), s0_remOlderMatchMaskVec(rem)(i).reduce(_|_))
    })).asUInt
  }))
  val s0_remOldestHintSelVec = s0_remOldestSelVec.zip(s0_remLoadHintSelMask).map {
    case(oldestVec, hintVec) => oldestVec & hintVec
  }

  // select oldest logic
  s0_oldestSel := VecInit((0 until MlsPipelineWidth).map(rport => {
    // select enqueue earlest inst
    val ageOldest = AgeDetector(MlsQueueReplaySize / MlsPipelineWidth, s0_remEnqSelVec(rport), s0_remFreeSelVec(rport), s0_remPriorityReplaySelVec(rport))
    assert(!(ageOldest.valid && PopCount(ageOldest.bits) > 1.U), "oldest index must be one-hot!")
    val ageOldestValid = ageOldest.valid
    val ageOldestIndexOH = ageOldest.bits

    // select program order oldest
    val l2HintFirst = false.B
    val issOldestValid = l2HintFirst || ParallelORR(s0_remOldestSelVec(rport))
    val issOldestIndexOH = Mux(l2HintFirst, PriorityEncoderOH(s0_remOldestHintSelVec(rport)), PriorityEncoderOH(s0_remOldestSelVec(rport)))

    val oldest = Wire(Valid(UInt()))
    val oldestSel = Mux(issOldestValid, issOldestIndexOH, ageOldestIndexOH)
    val oldestBitsVec = Wire(Vec(MlsQueueReplaySize, Bool()))

    require((MlsQueueReplaySize % MlsPipelineWidth) == 0)
    oldestBitsVec.foreach(e => e := false.B)
    for (i <- 0 until MlsQueueReplaySize / MlsPipelineWidth) {
      oldestBitsVec(i * MlsPipelineWidth + rport) := oldestSel(i)
    }

    oldest.valid := ageOldest.valid || issOldestValid
    oldest.bits := oldestBitsVec.asUInt
    oldest
  }))

  // stage2: send replay request to load unit
  // replay cold down
  val ColdDownCycles = 16
  val coldCounter = RegInit(VecInit(List.fill(MlsPipelineWidth)(0.U(log2Up(ColdDownCycles).W))))
  val ColdDownThreshold = Wire(UInt(log2Up(ColdDownCycles).W))
  ColdDownThreshold := Constantin.createRecord(s"ColdDownThreshold_${p(XSCoreParamsKey).HartId}", initValue = 12)
  assert(ColdDownCycles.U > ColdDownThreshold, "ColdDownCycles must great than ColdDownThreshold!")

  def replayCanFire(i: Int) = coldCounter(i) >= 0.U && coldCounter(i) < ColdDownThreshold
  def coldDownNow(i: Int) = coldCounter(i) >= ColdDownThreshold

  val replay_req = Wire(Vec(MlsPipelineWidth, DecoupledIO(new MlsPipelineBundle)))

  for (i <- 0 until MlsPipelineWidth) {
    val s0_can_go = s1_can_go(i) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(RegNext(io.redirect))
    val s0_oldestSelIndexOH = s0_oldestSel(i).bits // one-hot
    s1_oldestSel(i).valid := RegEnable(s0_oldestSel(i).valid, false.B, s0_can_go)
    s1_oldestSel(i).bits := RegEnable(OHToUInt(s0_oldestSel(i).bits), s0_can_go)

    for (j <- 0 until MlsQueueReplaySize) {
      when (s0_can_go && s0_oldestSel(i).valid && s0_oldestSelIndexOH(j)) {
        scheduled(j) := true.B
      }
    }
  }
  val s2_cancelReplay = Wire(Vec(MlsPipelineWidth, Bool()))
  for (i <- 0 until MlsPipelineWidth) {
    val s1_cancel = uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(RegNext(io.redirect))
    val s1_oldestSelV = s1_oldestSel(i).valid && !s1_cancel
    s1_can_go(i)          := replayCanFire(i) && (!s2_oldestSel(i).valid || replay_req(i).fire) || s2_cancelReplay(i)
    s2_oldestSel(i).valid := RegEnable(Mux(s1_can_go(i), s1_oldestSelV, false.B), false.B, (s1_can_go(i) || replay_req(i).fire))
    s2_oldestSel(i).bits  := RegEnable(s1_oldestSel(i).bits, s1_can_go(i))

    vaddrModule.io.ren(i) := s1_oldestSel(i).valid && s1_can_go(i)
    vaddrModule.io.raddr(i) := s1_oldestSel(i).bits
  }

  for (i <- 0 until MlsPipelineWidth) {
    val s1_replayIdx = s1_oldestSel(i).bits
    val s2_replayUop = RegEnable(uop(s1_replayIdx), s1_can_go(i))
    val s2_vecReplay = RegEnable(vecReplay(s1_replayIdx), s1_can_go(i))
    val s2_missDbUpdated = RegEnable(missDbUpdated(s1_replayIdx), s1_can_go(i))
    val s2_replayCauses = RegEnable(cause(s1_replayIdx), s1_can_go(i))
    s2_cancelReplay(i) := s2_replayUop.robIdx.needFlush(io.redirect)

    s2_can_go(i) := DontCare
    replay_req(i).valid             := s2_oldestSel(i).valid
    replay_req(i).bits              := DontCare
    replay_req(i).bits.uop          := s2_replayUop
    replay_req(i).bits.uop.exceptionVec(loadAddrMisaligned) := false.B
    replay_req(i).bits.isvec        := s2_vecReplay.isvec
    replay_req(i).bits.isLastElem   := s2_vecReplay.isLastElem
    replay_req(i).bits.is128bit     := s2_vecReplay.is128bit
    replay_req(i).bits.uop_unit_stride_fof := s2_vecReplay.uop_unit_stride_fof
    replay_req(i).bits.usSecondInv  := s2_vecReplay.usSecondInv
    replay_req(i).bits.elemIdx      := s2_vecReplay.elemIdx
    replay_req(i).bits.alignedType  := s2_vecReplay.alignedType
    replay_req(i).bits.mbIndex      := s2_vecReplay.mbIndex
    replay_req(i).bits.elemIdxInsideVd := s2_vecReplay.elemIdxInsideVd
    replay_req(i).bits.reg_offset   := s2_vecReplay.reg_offset
    replay_req(i).bits.vecActive    := s2_vecReplay.vecActive
    replay_req(i).bits.is_first_ele := s2_vecReplay.is_first_ele
    replay_req(i).bits.mask         := s2_vecReplay.mask
    replay_req(i).bits.vaddr        := vaddrModule.io.rdata(i)
    replay_req(i).bits.isFirstIssue := false.B
    replay_req(i).bits.isLoadReplay := true.B
    replay_req(i).bits.replayCarry  := 0.U.asTypeOf(replay_req(i).bits.replayCarry) // TODO: remove me?
    replay_req(i).bits.mshrid       := 0.U // TODO: remove me?
    replay_req(i).bits.replacementUpdated := false.B // TODO: remove me?
    replay_req(i).bits.missDbUpdated := s2_missDbUpdated
    replay_req(i).bits.forward_tlDchannel := false.B
    replay_req(i).bits.schedIndex   := s2_oldestSel(i).bits
    replay_req(i).bits.uop.loadWaitStrict := false.B
    replay_req(i).bits.stride       := s2_vecReplay.stride
    replay_req(i).bits.mtile0       := s2_vecReplay.mtile0
    replay_req(i).bits.mtile1       := s2_vecReplay.mtile1

    XSError(replay_req(i).fire && !allocated(s2_oldestSel(i).bits), p"LoadQueueReplay: why replay an invalid entry ${s2_oldestSel(i).bits} ?")
  }

  val EnableHybridUnitReplay = Constantin.createRecord("EnableHybridUnitReplay", true)
  
  for (i <- 0 until MlsPipelineWidth) {
    io.replay(i) <> replay_req(i)
  }
  // update cold counter
  val lastReplay = RegNext(VecInit(io.replay.map(_.fire)))
  for (i <- 0 until MlsPipelineWidth) {
    when (lastReplay(i) && io.replay(i).fire) {
      coldCounter(i) := coldCounter(i) + 1.U
    } .elsewhen (coldDownNow(i)) {
      coldCounter(i) := coldCounter(i) + 1.U
    } .otherwise {
      coldCounter(i) := 0.U
    }
  }

  // XSDebug(io.refill.valid, "miss resp: paddr:0x%x data %x\n", io.refill.bits.addr, io.refill.bits.data)


  // init
  freeMaskVec.map(e => e := false.B)

  // LoadQueueReplay can't backpressure.
  // We think LoadQueueReplay can always enter, as long as it is the same size as VirtualLoadQueue.
  assert(freeList.io.canAllocate.reduce(_ || _) || !io.enq.map(_.valid).reduce(_ || _), s"LoadQueueReplay Overflow")

  // Allocate logic
  needEnqueue.zip(newEnqueue).zip(io.enq).map {
    case ((needEnq, newEnq), enq) =>
      newEnq := needEnq && !enq.bits.isLoadReplay
  }

  for ((enq, w) <- io.enq.zipWithIndex) {
    vaddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocated ready
    val offset = PopCount(newEnqueue.take(w))
    val enqIndex = Mux(enq.bits.isLoadReplay, enq.bits.schedIndex, freeList.io.allocateSlot(offset))
    enqIndexOH(w) := UIntToOH(enqIndex)
    enq.ready := true.B

    val debug_robIdx = enq.bits.uop.robIdx.asUInt
    XSError(
      needEnqueue(w) && enq.ready &&
      allocated(enqIndex) && !enq.bits.isLoadReplay,
      p"LoadQueueReplay: can not accept more load, check: ldu $w, robIdx $debug_robIdx!")
    XSError(
      needEnqueue(w) && enq.ready &&
      hasExceptions(w),
      p"LoadQueueReplay: The instruction has exception, it can not be replay, check: ldu $w, robIdx $debug_robIdx!")
    when (needEnqueue(w) && enq.ready) {
      freeList.io.doAllocate(w) := !enq.bits.isLoadReplay

      //  Allocate new entry
      allocated(enqIndex) := true.B
      scheduled(enqIndex) := false.B
      uop(enqIndex)       := enq.bits.uop
      vecReplay(enqIndex).isvec := enq.bits.isvec
      vecReplay(enqIndex).isLastElem := enq.bits.isLastElem
      vecReplay(enqIndex).is128bit := enq.bits.is128bit
      vecReplay(enqIndex).uop_unit_stride_fof := enq.bits.uop_unit_stride_fof
      vecReplay(enqIndex).usSecondInv := enq.bits.usSecondInv
      vecReplay(enqIndex).elemIdx := enq.bits.elemIdx
      vecReplay(enqIndex).alignedType:= enq.bits.alignedType
      vecReplay(enqIndex).mbIndex := enq.bits.mbIndex
      vecReplay(enqIndex).elemIdxInsideVd := enq.bits.elemIdxInsideVd
      vecReplay(enqIndex).reg_offset := enq.bits.reg_offset
      vecReplay(enqIndex).vecActive := enq.bits.vecActive
      vecReplay(enqIndex).is_first_ele := enq.bits.is_first_ele
      vecReplay(enqIndex).mask         := enq.bits.mask
      vecReplay(enqIndex).stride       := enq.bits.stride
      vecReplay(enqIndex).mtile0       := enq.bits.mtile0
      vecReplay(enqIndex).mtile1       := enq.bits.mtile1

      vaddrModule.io.wen(w)   := true.B
      vaddrModule.io.waddr(w) := enqIndex
      vaddrModule.io.wdata(w) := enq.bits.vaddr
      debug_vaddr(enqIndex)   := enq.bits.vaddr

      /**
       * used for feedback and replay
       */
      // set flags
      val replayInfo = enq.bits.rep_info
      val dataInLastBeat = replayInfo.last_beat
      cause(enqIndex) := replayInfo.cause.asUInt


      // init
      blocking(enqIndex)     := true.B

      // special case: tlb miss
      when (replayInfo.cause(MlsReplayCauses.C_TM)) {
        blocking(enqIndex) := !replayInfo.tlb_full &&
          !(io.tlb_hint.resp.valid && (io.tlb_hint.resp.bits.id === replayInfo.tlb_id || io.tlb_hint.resp.bits.replay_all))
        tlbHintId(enqIndex) := replayInfo.tlb_id
      }

      // extra info
      missDbUpdated(enqIndex) := enq.bits.missDbUpdated
    }

    //
    val schedIndex = enq.bits.schedIndex
    when (enq.valid && enq.bits.isLoadReplay) {
      when (!needReplay(w) || hasExceptions(w)) {
        allocated(schedIndex) := false.B
        freeMaskVec(schedIndex) := true.B
      } .otherwise {
        scheduled(schedIndex) := false.B
      }
    }
  }

  // misprediction recovery / exception redirect
  for (i <- 0 until MlsQueueReplaySize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.redirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
    }
  }

  freeList.io.free := freeMaskVec.asUInt

  io.mlsqFull := mlsqFull
  // end
}