package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend._
import xiangshan.backend.fu.FuConfig.MlsCfg
import xiangshan.backend.rob._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.mem.Bundles.{MlsqWriteBundle, MlsPipelineBundle}
import utils._
import utility._

class VirtualMlsQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasVLSUParameters
{
  val io = IO(new Bundle {
    // control
    val redirect = Flipped(ValidIO(new Redirect))
    // from dispatch
    val enq = new MlsEnqIO
    // from mlsu s3
    val lsin = Vec(backendParams.MlsCnt, Flipped(Decoupled(new MlsqWriteBundle)))
    // to LoadQueueReplay and LoadQueueRAR
    val mlsWbPtr = Output(new MlsqPtr)
    // global
    val mlsqFull = Output(Bool())
    val mlsqEmpty = Output(Bool())
    // to dispatch
    val mlsqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val mlsqCancelCnt = Output(UInt(log2Up(MlsQueueSize + 1).W))
  })

  println("VirtualMlsLoadQueue: size: " + VirtualMlsQueueSize)
  //  VirtualLoadQueue field
  //  +-----------+---------+-------+
  //  | Allocated | MicroOp | Flags |
  //  +-----------+---------+-------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  Flags       : load flags
  val allocated = RegInit(VecInit(List.fill(VirtualMlsQueueSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val robIdx = Reg(Vec(VirtualMlsQueueSize, new RobPtr))
  val committed = Reg(Vec(VirtualMlsQueueSize, Bool()))

  //  maintain pointers
  val enqPtrExt = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new MlsqPtr))))
  val enqPtr = enqPtrExt(0).value
  val deqPtr = Wire(new MlsqPtr)
  val deqPtrNext = Wire(new MlsqPtr)

  /**
   * update pointer
   */
  val lastCycleRedirect = RegNext(io.redirect)
  val lastLastCycleRedirect = RegNext(lastCycleRedirect)

  val validCount = distanceBetween(enqPtrExt(0), deqPtr)
  val allowEnqueue = validCount <= (MlsQueueSize - LSQMlsEnqWidth).U
  val canEnqueue = io.enq.req.map(_.valid)
  val needCancel = WireInit(VecInit((0 until VirtualMlsQueueSize).map(i => {
    robIdx(i).needFlush(io.redirect) && allocated(i)
  })))
  val lastNeedCancel = GatedValidRegNext(needCancel)
  val enqCancel = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancel.zip(io.enq.req).map{case (v, req) =>
    Mux(v, req.bits.numLsElem, 0.U)
  }
  val lastEnqCancel = GatedRegNext(enqCancelNum.reduce(_ + _))
  val lastCycleCancelCount = PopCount(lastNeedCancel)
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect.valid)

  // update enqueue pointer
  val vLoadFlow = io.enq.req.map(_.bits.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val validVLoadFlow = vLoadFlow.zipWithIndex.map{case (vLoadFlowNumItem, index) => Mux(canEnqueue(index), vLoadFlowNumItem, 0.U)}
  val validVLoadOffset = vLoadFlow.zip(io.enq.needAlloc).map{case (flow, needAllocItem) => Mux(needAllocItem, flow, 0.U)}
  val validVLoadOffsetRShift = 0.U +: validVLoadOffset.take(validVLoadFlow.length - 1)

  val enqNumber = validVLoadFlow.reduce(_ + _)
  val enqPtrExtNextVec = Wire(Vec(io.enq.req.length, new MlsqPtr))
  val enqPtrExtNext = Wire(Vec(io.enq.req.length, new MlsqPtr))
  when (lastLastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  } .otherwise {
    enqPtrExtNextVec := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect.valid && enqNumber =/= 0.U))

  when (isAfter(enqPtrExtNextVec(0), deqPtrNext)) {
    enqPtrExtNext := enqPtrExtNextVec
  } .otherwise {
    enqPtrExtNext := VecInit((0 until io.enq.req.length).map(i => deqPtrNext + i.U))
  }
  enqPtrExt := enqPtrExtNext

  // update dequeue pointer
  val DeqPtrMoveStride = CommitWidth
  val deqLookupVec = VecInit((0 until DeqPtrMoveStride).map(deqPtr + _.U))
  val deqLookup = VecInit(deqLookupVec.map(ptr => allocated(ptr.value) && committed(ptr.value) && ptr =/= enqPtrExt(0)))
  val deqInSameRedirectCycle = VecInit(deqLookupVec.map(ptr => needCancel(ptr.value)))
  // make chisel happy
  val deqCountMask = Wire(UInt(DeqPtrMoveStride.W))
  deqCountMask := deqLookup.asUInt & (~deqInSameRedirectCycle.asUInt).asUInt
  val commitCount = PopCount(PriorityEncoderOH(~deqCountMask) - 1.U)
  val lastCommitCount = GatedRegNext(commitCount)

  // update deqPtr
  // cycle 1: generate deqPtrNext
  // cycle 2: update deqPtr
  val deqPtrUpdateEna = lastCommitCount =/= 0.U
  deqPtrNext := deqPtr + lastCommitCount
  deqPtr := RegEnable(deqPtrNext, 0.U.asTypeOf(new MlsqPtr), deqPtrUpdateEna)

  io.mlsqDeq := GatedRegNext(lastCommitCount)
  io.mlsqCancelCnt := redirectCancelCount
  io.mlsWbPtr := deqPtr
  io.mlsqFull := !allowEnqueue
  io.mlsqEmpty := RegNext(validCount === 0.U)

  io.enq.canAccept := allowEnqueue
  val enqLowBound = io.enq.req.map(_.bits.mlsqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.mlsqIdx + x.bits.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}

  for (i <- 0 until VirtualMlsQueueSize) {
    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }
    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))
    when (entryCanEnq) {
      allocated(i) := true.B
      robIdx(i) := selectBits.robIdx
      committed(i) := false.B
    }
  }

  for (i <- 0 until io.enq.req.length) {
    val mlsqIdx = enqPtrExt(0) + validVLoadOffsetRShift.take(i + 1).reduce(_ + _)
    val index = io.enq.req(i).bits.mlsqIdx
    // XSError(canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.sqCanAccept), s"must accept $i\n")
    XSError(canEnqueue(i) && !enqCancel(i) && index.value =/= mlsqIdx.value, s"must be the same entry $i\n")
    io.enq.resp(i) := mlsqIdx
  }

  /**
    * Matrix Load/Store commits
    *
    * When load/store commited, mark it as !allocated and move deqPtr forward.
    */
  (0 until DeqPtrMoveStride).map(i => {
    when (commitCount > i.U) {
      allocated((deqPtr+i.U).value) := false.B
    }
    XSError(commitCount > i.U && !allocated((deqPtr+i.U).value), s"why commit invalid entry $i?\n")
  })

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  for (i <- 0 until VirtualMlsQueueSize) {
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  /**
    * Writeback load from load units
    *
    * Most load instructions writeback to regfile at the same time.
    * However,
    *   (1) For ready load instruction (no need replay), it writes back to ROB immediately.
    */
  for(i <- 0 until MlsPipelineWidth) {
    //   most lq status need to be updated immediately after load writeback to lq
    //   flag bits in lq needs to be updated accurately
    io.lsin(i).ready := true.B
    val loadWbIndex = io.lsin(i).bits.uop.mlsqIdx.value

    when (io.lsin(i).valid) {
      val hasExceptions = ExceptionNO.selectByFu(io.lsin(i).bits.uop.exceptionVec, MlsCfg).asUInt.orR
      val need_rep = io.lsin(i).bits.rep_info.need_rep
      val need_valid = io.lsin(i).bits.updateAddrValid

      when (!need_rep && need_valid) {
        committed(loadWbIndex) := true.B
      }

      XSInfo(!need_rep && need_valid,
        "matrix load/store hit write to lq idx %d pc 0x%x vaddr %x paddr %x mask %x forwardData %x forwardMask: %x mmio %x\n",
        io.lsin(i).bits.uop.mlsqIdx.asUInt,
        io.lsin(i).bits.uop.pc,
        io.lsin(i).bits.vaddr,
        io.lsin(i).bits.paddr,
        io.lsin(i).bits.mask,
        io.lsin(i).bits.forwardData.asUInt,
        io.lsin(i).bits.forwardMask.asUInt,
        io.lsin(i).bits.mmio
      )
    }
  }
}