package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.fu.matrix.Bundles._

class AmuCtrlBufferIO()(implicit val p: Parameters, val params: BackendParams) extends Bundle with HasXSParameter {
  // rob enq
  class EnqBundle extends Bundle {
    val valid = Bool()  // Req valid and canEnqueue
    val reqValid = Bool()  // Req valid
    val allocPtr = new RobPtr
    val needAMU = Bool()
  }
  val enq = Input(Vec(RenameWidth, new EnqBundle))
  val canEnqueue = Output(Bool())
  val canEnqueueForDispatch = Output(Bool())

  // rob wb
  val wb = Flipped(params.genWrite2CtrlBundles)

  // Commit
  class DeqCommitBundle extends Bundle {
    val ptr = new RobPtr
    val valid = Bool()
  }
  val deqCommit = Input(Vec(CommitWidth, new DeqCommitBundle))

  // Redirect (no need to walk)
  class RedirectBundle extends Bundle {
    val valid = Bool()
    val all = Bool()
    val begin = UInt(log2Up(RobSize).W)
    val end = UInt(log2Up(RobSize).W)
  }
  val redirect = Input(new RedirectBundle)

  // To Amu
  val toAMU = Vec(CommitWidth, DecoupledIO(new AmuCtrlIO))
}


class AmuCtrlEntry(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val needAMU = Bool()
  val writebacked = Bool()
  val committed = Bool()
  val amuCtrl = new AmuCtrlIO
  val canDeq = Bool()

  def amuReqValid = valid && needAMU && writebacked && committed && !canDeq

  def checkSanity(name: String) = {
    // Check valid sequence: canDeq -> committed -> writebacked -> valid
    assert(!canDeq || committed, s"$name: committed must be valid when canDeq is valid")
    assert(!committed || (!needAMU || writebacked), s"$name: writebacked must be valid when committed is valid") 
    assert(!committed || valid, s"$name: valid must be valid when committed is valid")
    assert(!needAMU || valid, s"$name: valid must be valid when needAMU is valid")
    assert(!writebacked || valid, s"$name: valid must be valid when writebacked is valid")
  }
}

object AmuCtrlEntry {
  def apply()(implicit p: Parameters) = {
    new AmuCtrlEntry
  }

  def zero(implicit p: Parameters) = {
    val default_zero = 0.U.asTypeOf(new AmuCtrlEntry)
    assert(default_zero.getWidth > 0)
    default_zero
  }
}

/**
 * AMU Control Buffer
 *
 * Purpose:
 * A dedicated buffer parallel to ROB for storing AMU control signals, avoiding storing them in ROB directly.
 * The buffer size matches ROB size.
 *
 * Operation:
 * 1. Instructions enter this buffer synchronously when entering ROB
 * 2. When buffer is full, it will backpressure ROB enqueue
 * 3. Synchronizes with ROB writeback and flush, no need for walking to rebuild context
 * 4. After ROB commit, starts handshake with AMU and dequeues after handshake fires
 */
class AmuCtrlBuffer()(implicit override val p: Parameters, val params: BackendParams) extends XSModule
  with HasXSParameter with HasCircularQueuePtrHelper {

  val io = IO(new AmuCtrlBufferIO)

  def connectROB(robImp: RobImp) = {
    for (i <- 0 until RenameWidth) {
      io.enq(i).valid := robImp.io.enq.req(i).valid && robImp.canEnqueue(i) && !robImp.io.redirect.valid
      io.enq(i).reqValid := robImp.io.enq.req(i).valid
      io.enq(i).allocPtr := robImp.allocatePtrVec(i)
      io.enq(i).needAMU := robImp.io.enq.req(i).bits.needAmuCtrl
    }
    io.wb := robImp.io.writeback
    io.deqCommit.zip(robImp.deqPtrVec).zip(robImp.io.commits.commitValid) foreach { case ((deq, ptr), valid) =>
      deq.ptr := ptr
      deq.valid := valid && robImp.io.commits.isCommit
    }
    io.redirect.valid := robImp.redirectValidReg
    io.redirect.all := robImp.redirectAll
    io.redirect.begin := robImp.redirectBegin
    io.redirect.end := robImp.redirectEnd
    robImp.io.amuCtrl <> io.toAMU
  }

  val amuCtrlEntries = RegInit(VecInit.fill(RobSize)(AmuCtrlEntry.zero))

  // Calculate number of valid entries and new entries to be enqueued
  val numValidEntries = PopCount(amuCtrlEntries.map(_.valid))
  val numNewEntries = PopCount(io.enq.map(_.reqValid))

  // Check if there's enough space in the queue
  val canEnqueue = GatedValidRegNext(
    numValidEntries + numNewEntries <= (RobSize - RenameWidth).U,
    true.B
  )
  val canEnqueueForDispatch = GatedValidRegNext(
    numValidEntries + numNewEntries <= (RobSize - RenameWidth * 2).U,
    true.B
  )
  io.canEnqueue := canEnqueue
  io.canEnqueueForDispatch := canEnqueueForDispatch

  // Enqueue (Sync with outer ROB)
  // DynInst does not carry amuCtrl info,
  // so we only need to mark valid for entries who need amuCtrl.
  for (i <- 0 until RobSize) {
    val indexMatch = io.enq.map(_.allocPtr.value === i.U)
    val enqOH = VecInit(io.enq.zip(indexMatch).map(x => x._1.valid && x._2))
    val needOH = VecInit(io.enq.zip(indexMatch).map(x => x._1.needAMU && x._2))
    val entry = amuCtrlEntries(i)
    when (enqOH.asUInt.orR) {
      entry := AmuCtrlEntry.zero
      entry.valid := true.B
      entry.needAMU := needOH.asUInt.orR
    }
  }

  // Writeback (Sync with outer ROB)
  val amuCtrlWb = io.wb.filter(_.bits.amuCtrl.nonEmpty).toSeq
  for (i <- 0 until RobSize) {
    val amu_data = amuCtrlWb.map{ wb =>
      val valid_match = wb.valid && wb.bits.robIdx.value === i.U
      Mux(valid_match, wb.bits.amuCtrl.get.asUInt, 0.U)
    }.reduce(_ | _)

    val entry = amuCtrlEntries(i)
    assert(!amu_data.orR || entry.valid, s"AMUCtrl entry $i is invalid but has amu_data")
    when (amu_data.orR) {
      entry.writebacked := true.B
      entry.amuCtrl := amu_data.asTypeOf(new AmuCtrlIO)
    }
  }

  // Commit (Sync with outer ROB)
  for (i <- 0 until RobSize) {
    val deqValid = io.deqCommit.map(x => x.ptr.value === i.U && x.valid)
    val commitCond = deqValid.reduce(_ || _)
    when (commitCond) {
      assert(amuCtrlEntries(i).valid, s"AMUCtrlBuffer: amuCtrlEntries[$i] is invalid but will commit")
      amuCtrlEntries(i).committed := true.B
      when (!amuCtrlEntries(i).needAMU) {
        amuCtrlEntries(i).canDeq := true.B
      }
    }
  }

  // Redirect (Sync with outer ROB)
  for (i <- 0 until RobSize) {
    val needFlush = io.redirect.valid &&
      Mux((io.redirect.end > io.redirect.begin) && !io.redirect.all,
        (i.U > io.redirect.begin) && (i.U < io.redirect.end),
        (i.U > io.redirect.begin) || (i.U < io.redirect.end)
    )

    when (needFlush) {
      amuCtrlEntries(i) := AmuCtrlEntry.zero
    }
  }

  // To AMU
  val deqPtr = RegInit(0.U.asTypeOf(new RobPtr))
  val deqEntries = (0 until CommitWidth).map(i => amuCtrlEntries((deqPtr + i.U).value))
  io.toAMU.zipWithIndex.foreach { case (amuCtrl, i) =>
    val deqEntry = deqEntries(i)
    deqEntry.checkSanity(s"AMUCtrlBuffer: deqEntry[$i]")
    amuCtrl.valid := deqEntry.amuReqValid
    amuCtrl.bits := Mux(deqEntry.amuReqValid, deqEntry.amuCtrl, 0.U.asTypeOf(new AmuCtrlIO))
    when (amuCtrl.fire) {
      assert(!deqEntry.canDeq, s"AMUCtrlBuffer: deqEntry[$i] is already set to canDeq")
      deqEntry.canDeq := true.B
    }
  }

  val deqCondSeq = deqEntries.map(x => ~x.canDeq)
  val deqCount = PriorityEncoder(deqCondSeq)
  // Update deqPtr and invalidate entries
  deqPtr := deqPtr + deqCount
  for (i <- 0 until CommitWidth) {
    val curr_deq_ptr = deqPtr + i.U
    when(amuCtrlEntries(curr_deq_ptr.value).canDeq && i.U < deqCount) {
      amuCtrlEntries(curr_deq_ptr.value) := AmuCtrlEntry.zero
    }
  }

  XSPerfAccumulate("stall_by_matrix_fire", !io.canEnqueue && deqPtr =/= io.deqCommit(0).ptr)
}