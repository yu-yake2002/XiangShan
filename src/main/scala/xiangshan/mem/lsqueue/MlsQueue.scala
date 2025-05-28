package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, MemMicroOpRbExt}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.Bundles.{MlsqWriteBundle, MlsPipelineBundle}

class MlsqPtr(implicit p: Parameters) extends CircularQueuePtr[MlsqPtr](
  p => p(XSCoreParamsKey).MlsQueueSize
){
}

object MlsqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): MlsqPtr = {
    val ptr = Wire(new MlsqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}
class MlsEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(Bool()))
  val req = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
  val resp = Vec(LSQEnqWidth, Output(new MlsqPtr))
}

class MlsQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val enq = new MlsEnqIO

    val mlsu = new Bundle(){
      val mlsin = Vec(backendParams.MlsCnt, Flipped(Decoupled(new MlsqWriteBundle)))
    }

    val replay = Vec(backendParams.MlsCnt, Decoupled(new MlsPipelineBundle))

    val mlsqFull = Output(Bool())
    val mlsq_rep_full = Output(Bool())
    val mlsqEmpty = Output(Bool())
    val mlsqDeqPtr = Output(new MlsqPtr)
    val mlsqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val mlsqCancelCnt = Output(UInt(log2Up(MlsQueueSize + 1).W))

    val tlb_hint = Flipped(new TlbHintIO)
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))
  })

  val mlsQueueReplay = Module(new MlsQueueReplay)
  val virtualMlsQueue = Module(new VirtualMlsQueue)

  /**
   * VirtualLoadQueue
   */
  virtualMlsQueue.io.redirect      <> io.redirect
  virtualMlsQueue.io.enq           <> io.enq
  virtualMlsQueue.io.lsin          <> io.mlsu.mlsin
  virtualMlsQueue.io.mlsqFull      <> io.mlsqFull
  virtualMlsQueue.io.mlsqDeq       <> io.mlsqDeq
  virtualMlsQueue.io.mlsqCancelCnt <> io.mlsqCancelCnt
  virtualMlsQueue.io.mlsqEmpty     <> io.mlsqEmpty
  virtualMlsQueue.io.mlsWbPtr      <> io.mlsqDeqPtr

  mlsQueueReplay.io.redirect                <> io.redirect
  mlsQueueReplay.io.enq                     <> io.mlsu.mlsin // from loadstore_s3
  mlsQueueReplay.io.replay                  <> io.replay
  mlsQueueReplay.io.mlsqFull                <> io.mlsq_rep_full
  mlsQueueReplay.io.mlsWbPtr                <> virtualMlsQueue.io.mlsWbPtr
  mlsQueueReplay.io.tlb_hint                <> io.tlb_hint
  mlsQueueReplay.io.tlbReplayDelayCycleCtrl <> io.tlbReplayDelayCycleCtrl
}