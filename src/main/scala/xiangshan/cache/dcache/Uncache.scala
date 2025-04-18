/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils._
import utility._
import xiangshan._
import xiangshan.mem._
import coupledL2.MemBackTypeMM
import coupledL2.MemPageTypeNC
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLArbiter, TLBundleA, TLBundleD, TLClientNode, TLEdgeOut, TLMasterParameters, TLMasterPortParameters}
import coupledL2.{MemBackTypeMMField, MemPageTypeNCField}

class UncacheFlushBundle extends Bundle {
  val valid = Output(Bool())
  val empty = Input(Bool())
}

class UncacheEntry(implicit p: Parameters) extends DCacheBundle {
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val data = UInt(XLEN.W)
  val mask = UInt(DataBytes.W)
  val id = UInt(uncacheIdxBits.W)
  val nc = Bool()
  val atomic = Bool()
  val memBackTypeMM = Bool()

  val resp_nderr = Bool()

  /* NOTE: if it support the internal forward logic, here can uncomment */
  // val fwd_data = UInt(XLEN.W)
  // val fwd_mask = UInt(DataBytes.W)

  def set(x: UncacheWordReq): Unit = {
    cmd := x.cmd
    addr := x.addr
    vaddr := x.vaddr
    data := x.data
    mask := x.mask
    id := x.id
    nc := x.nc
    memBackTypeMM := x.memBackTypeMM
    atomic := x.atomic
    resp_nderr := false.B
    // fwd_data := 0.U
    // fwd_mask := 0.U
  }

  def update(x: TLBundleD): Unit = {
    when(cmd === MemoryOpConstants.M_XRD) {
      data := x.data
    }
    resp_nderr := x.denied
  }

  // def update(forwardData: UInt, forwardMask: UInt): Unit = {
  //   fwd_data := forwardData
  //   fwd_mask := forwardMask
  // }

  def toUncacheWordResp(): UncacheWordResp = {
    // val resp_fwd_data = VecInit((0 until DataBytes).map(j =>
    //   Mux(fwd_mask(j), fwd_data(8*(j+1)-1, 8*j), data(8*(j+1)-1, 8*j))
    // )).asUInt
    val resp_fwd_data = data
    val r = Wire(new UncacheWordResp)
    r := DontCare
    r.data := resp_fwd_data
    r.id := id
    r.nderr := resp_nderr
    r.nc := nc
    r.is2lq := cmd === MemoryOpConstants.M_XRD
    r.miss := false.B
    r.replay := false.B
    r.tag_error := false.B
    r.error := false.B
    r
  }
}

class UncacheEntryState(implicit p: Parameters) extends DCacheBundle {
  // valid (-> waitSame) -> inflight -> waitReturn
  val valid = Bool()
  val inflight = Bool() // uncache -> L2
  val waitSame = Bool()
  val waitReturn = Bool() // uncache -> LSQ
  
  def init: Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }

  def isValid(): Bool = valid
  def isInflight(): Bool = inflight
  def isWaitReturn(): Bool = waitReturn
  def isWaitSame(): Bool = waitSame
  def can2Uncache(): Bool = valid && !inflight && !waitSame && !waitReturn
  def can2Lsq(): Bool = valid && waitReturn
  
  def setValid(x: Bool): Unit = { valid := x}
  def setInflight(x: Bool): Unit = { inflight := x}
  def setWaitReturn(x: Bool): Unit = { waitReturn := x }
  def setWaitSame(x: Bool): Unit = { waitSame := x}
  
  def updateUncacheResp(): Unit = {
    assert(inflight, "The request was not sent and a response was received")
    inflight := false.B
    waitReturn := true.B
  }
  def updateReturn(): Unit = {
    valid := false.B
    inflight := false.B
    waitSame := false.B
    waitReturn := false.B
  }
}

class UncacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt())
  val enableOutstanding = Input(Bool())
  val flush = Flipped(new UncacheFlushBundle)
  val lsq = Flipped(new UncacheWordIO)
  val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
}

// convert DCacheIO to TileLink
// for Now, we only deal with TL-UL

class Uncache()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  def idRange: Int = UncacheBufferSize

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, idRange)
    )),
    requestFields = Seq(MemBackTypeMMField(), MemPageTypeNCField())
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

/* Uncache Buffer */
class UncacheImp(outer: Uncache)extends LazyModuleImp(outer)
  with HasTLDump
  with HasXSParameter
  with HasPerfEvents
{
  private val INDEX_WIDTH = log2Up(UncacheBufferSize)
  println(s"Uncahe Buffer Size: $UncacheBufferSize entries")
  val io = IO(new UncacheIO)

  val (bus, edge) = outer.clientNode.out.head

  val req  = io.lsq.req
  val resp = io.lsq.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d
  val req_ready = WireInit(false.B)

  // assign default values to output signals
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare
  io.lsq.req.ready := req_ready
  io.lsq.resp.valid := false.B
  io.lsq.resp.bits := DontCare


  /******************************************************************
   * Data Structure
   ******************************************************************/

  val entries = Reg(Vec(UncacheBufferSize, new UncacheEntry))
  val states = RegInit(VecInit(Seq.fill(UncacheBufferSize)(0.U.asTypeOf(new UncacheEntryState))))
  val fence = RegInit(Bool(), false.B)
  val s_idle :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val uState = RegInit(s_idle)
  
  def sizeMap[T <: Data](f: Int => T) = VecInit((0 until UncacheBufferSize).map(f))
  def isStore(e: UncacheEntry): Bool = e.cmd === MemoryOpConstants.M_XWR
  def isStore(x: UInt): Bool = x === MemoryOpConstants.M_XWR
  def addrMatch(x: UncacheEntry, y: UncacheWordReq): Bool = x.addr(PAddrBits - 1, 3) === y.addr(PAddrBits - 1, 3)
  def addrMatch(x: UncacheWordReq, y: UncacheEntry): Bool = x.addr(PAddrBits - 1, 3) === y.addr(PAddrBits - 1, 3)
  def addrMatch(x: UncacheEntry, y: UncacheEntry): Bool = x.addr(PAddrBits - 1, 3) === y.addr(PAddrBits - 1, 3)
  def addrMatch(x: UInt, y: UInt): Bool = x(PAddrBits - 1, 3) === y(PAddrBits - 1, 3)

  // drain buffer
  val empty = Wire(Bool())
  val f1_needDrain = Wire(Bool())
  val do_uarch_drain = RegNext(f1_needDrain)

  val q0_entry = Wire(new UncacheEntry)
  val q0_canSentIdx = Wire(UInt(INDEX_WIDTH.W))
  val q0_canSent = Wire(Bool())


  /******************************************************************
   * uState for non-outstanding
   ******************************************************************/

  switch(uState){
    is(s_idle){
      when(req.fire){
        uState := s_refill_req
      }
    }
    is(s_refill_req){
      when(mem_acquire.fire){
        uState := s_refill_resp
      }
    }
    is(s_refill_resp){
      when(mem_grant.fire){
        uState := s_send_resp
      }
    }
    is(s_send_resp){
      when(resp.fire){
        uState := s_idle
      }
    }
  }


  /******************************************************************
   * Enter Buffer
   *  Version 0 (better timing)
   *    e0 judge: alloc/merge write vec
   *    e1 alloc
   *
   *  Version 1 (better performance)
   *    solved in one cycle for achieving the original performance.
   ******************************************************************/

  /**
    TODO lyq: how to merge
    1. same addr
    2. same cmd
    3. valid
    FIXME lyq: not merge now due to the following issues
    1. load cann't be merged
    2. how to merge store and response precisely
  */

  val e0_fire = req.fire
  val e0_req_valid = req.valid
  val e0_req = req.bits
  /**
    TODO lyq: block or wait or forward?
    NOW: strict block by same address; otherwise: exhaustive consideration is needed.
      - ld->ld wait
      - ld->st forward
      - st->ld forward
      - st->st block
  */
  val e0_existSame = sizeMap(j => e0_req_valid && states(j).isValid() && addrMatch(e0_req, entries(j))).asUInt.orR
  val e0_invalidVec = sizeMap(i => !states(i).isValid())
  val (e0_allocIdx, e0_canAlloc) = PriorityEncoderWithFlag(e0_invalidVec)
  val e0_alloc = e0_canAlloc && !e0_existSame && e0_fire
  req_ready := e0_invalidVec.asUInt.orR && !e0_existSame && !do_uarch_drain

  when (e0_alloc) {
    entries(e0_allocIdx).set(e0_req)
    states(e0_allocIdx).setValid(true.B)

    // judge whether wait same block: e0 & q0
    val waitSameVec = sizeMap(j =>
      e0_req_valid && states(j).isValid() && states(j).isInflight() && addrMatch(e0_req, entries(j))
    )
    val waitQ0 = q0_canSent && addrMatch(e0_req, q0_entry)
    when (waitSameVec.reduce(_ || _) || waitQ0) {
      states(e0_allocIdx).setWaitSame(true.B)
    }

  }


  /******************************************************************
   * Uncache Req
   *  Version 0 (better timing)
   *    q0: choose which one is sent
   *    q0: sent
   *
   *  Version 1 (better performance)
   *    solved in one cycle for achieving the original performance.
   *    NOTE: "Enter Buffer" & "Uncache Req" not a continuous pipeline,
   *          because there is no guarantee that mem_aquire will be always ready.
   ******************************************************************/

  val q0_canSentVec = sizeMap(i =>
    (io.enableOutstanding || uState === s_refill_req) &&
    states(i).can2Uncache()
  )
  val q0_res = PriorityEncoderWithFlag(q0_canSentVec)
  q0_canSentIdx := q0_res._1
  q0_canSent := q0_res._2
  q0_entry := entries(q0_canSentIdx)

  val size = PopCount(q0_entry.mask)
  val (lgSize, legal) = PriorityMuxWithFlag(Seq(
    1.U -> 0.U,
    2.U -> 1.U,
    4.U -> 2.U,
    8.U -> 3.U
  ).map(m => (size===m._1) -> m._2))
  assert(!(q0_canSent && !legal))

  val q0_load = edge.Get(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize
  )._2

  val q0_store = edge.Put(
    fromSource      = q0_canSentIdx,
    toAddress       = q0_entry.addr,
    lgSize          = lgSize,
    data            = q0_entry.data,
    mask            = q0_entry.mask
  )._2

  val q0_isStore = q0_entry.cmd === MemoryOpConstants.M_XWR

  mem_acquire.valid := q0_canSent
  mem_acquire.bits := Mux(q0_isStore, q0_store, q0_load)
  mem_acquire.bits.user.lift(MemBackTypeMM).foreach(_ := q0_entry.memBackTypeMM)
  mem_acquire.bits.user.lift(MemPageTypeNC).foreach(_ := q0_entry.nc)
  when(mem_acquire.fire){
    states(q0_canSentIdx).setInflight(true.B)

    // q0 should judge whether wait same block
    (0 until UncacheBufferSize).map(j =>
      when(states(j).isValid() && !states(j).isWaitReturn() && addrMatch(q0_entry, entries(j))){
        states(j).setWaitSame(true.B)
      }
    )
  }


  /******************************************************************
   * Uncache Resp
   ******************************************************************/

  val (_, _, refill_done, _) = edge.addr_inc(mem_grant)

  mem_grant.ready := true.B
  when (mem_grant.fire) {
    val id = mem_grant.bits.source
    entries(id).update(mem_grant.bits)
    states(id).updateUncacheResp()
    assert(refill_done, "Uncache response should be one beat only!")

    // remove state of wait same block
    (0 until UncacheBufferSize).map(j =>
      when(states(j).isValid() && states(j).isWaitSame() && addrMatch(entries(id), entries(j))){
        states(j).setWaitSame(false.B)
      }
    )
  }


  /******************************************************************
   * Return to LSQ
   ******************************************************************/

  val r0_canSentVec = sizeMap(i => states(i).can2Lsq())
  val (r0_canSentIdx, r0_canSent) = PriorityEncoderWithFlag(r0_canSentVec)
  resp.valid := r0_canSent
  resp.bits := entries(r0_canSentIdx).toUncacheWordResp()
  when(resp.fire){
    states(r0_canSentIdx).updateReturn()
  }


  /******************************************************************
   * Buffer Flush
   * 1. when io.flush.valid is true: drain store queue and ubuffer
   * 2. when io.lsq.req.bits.atomic is true: not support temporarily
   ******************************************************************/
  empty := !VecInit(states.map(_.isValid())).asUInt.orR
  io.flush.empty := empty


  /******************************************************************
   * Load Data Forward
   *
   * 0. ld in ldu pipeline
   *    f0: vaddr match, mask & data select, fast resp
   *    f1: paddr match, resp
   *
   * 1. ld in buffer (in "Enter Buffer")
   *    ld(en) -> st(in): ld entry.update, state.updateUncacheResp
   *    st(en) -> ld(in): ld entry.update, state.updateUncacheResp
   *    NOW: strict block by same address; there is no such forward.
   *
   ******************************************************************/

  val f0_validMask = sizeMap(i => isStore(entries(i)) && states(i).isValid())
  val f0_fwdMaskCandidates = VecInit(entries.map(e => e.mask))
  val f0_fwdDataCandidates = VecInit(entries.map(e => e.data))
  val f1_tagMismatchVec = Wire(Vec(LoadPipelineWidth, Bool()))
  f1_needDrain := f1_tagMismatchVec.asUInt.orR && !empty

  for ((forward, i) <- io.forward.zipWithIndex) {
    val f0_fwdValid = forward.valid
    val f1_fwdValid = RegNext(f0_fwdValid)

    // f0 vaddr match
    val f0_vtagMatches = sizeMap(w => addrMatch(entries(w).vaddr, forward.vaddr))
    val f0_validTagMatches = sizeMap(w => f0_vtagMatches(w) && f0_validMask(w) && f0_fwdValid)
    // f0 select
    val f0_fwdMask = shiftMaskToHigh(
      forward.vaddr,
      Mux1H(f0_validTagMatches, f0_fwdMaskCandidates)
    ).asTypeOf(Vec(VDataBytes, Bool()))
    val f0_fwdData = shiftDataToHigh(
      forward.vaddr,
      Mux1H(f0_validTagMatches, f0_fwdDataCandidates)
    ).asTypeOf(Vec(VDataBytes, UInt(8.W)))

    // f1 paddr match
    val f1_fwdMask = RegEnable(f0_fwdMask, f0_fwdValid)
    val f1_fwdData = RegEnable(f0_fwdData, f0_fwdValid)
    // forward.paddr from dtlb, which is far from uncache
    val f1_ptagMatches = sizeMap(w => addrMatch(RegEnable(entries(w).addr, f0_fwdValid), RegEnable(forward.paddr, f0_fwdValid)))
    f1_tagMismatchVec(i) := sizeMap(w =>
      RegEnable(f0_vtagMatches(w), f0_fwdValid) =/= f1_ptagMatches(w) && RegEnable(f0_validMask(w), f0_fwdValid) && f1_fwdValid
    ).asUInt.orR
    when(f1_tagMismatchVec(i)) {
      XSDebug("forward tag mismatch: pmatch %x vmatch %x vaddr %x paddr %x\n",
        f1_ptagMatches.asUInt,
        RegEnable(f0_vtagMatches.asUInt, f0_fwdValid),
        RegEnable(forward.vaddr, f0_fwdValid),
        RegEnable(forward.paddr, f0_fwdValid)
      )
    }
    // f1 output
    forward.addrInvalid := false.B // addr in ubuffer is always ready
    forward.dataInvalid := false.B // data in ubuffer is always ready
    forward.matchInvalid := f1_tagMismatchVec(i) // paddr / vaddr cam result does not match
    for (j <- 0 until VDataBytes) {
      forward.forwardMaskFast(j) := f0_fwdMask(j)

      forward.forwardData(j) := f1_fwdData(j)
      forward.forwardMask(j) := false.B
      when(f1_fwdMask(j) && f1_fwdValid) {
        forward.forwardMask(j) := true.B
      }
    }

  }


  /******************************************************************
   * Debug / Performance
   ******************************************************************/

  /* Debug Counters */
  // print all input/output requests for debug purpose
  // print req/resp
  XSDebug(req.fire, "req cmd: %x addr: %x data: %x mask: %x\n",
    req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask)
  XSDebug(resp.fire, "data: %x\n", req.bits.data)
  // print tilelink messages
  XSDebug(mem_acquire.valid, "mem_acquire valid, ready=%d ", mem_acquire.ready)
  mem_acquire.bits.dump(mem_acquire.valid)

  XSDebug(mem_grant.fire, "mem_grant fire ")
  mem_grant.bits.dump(mem_grant.fire)

  /* Performance Counters */
  XSPerfAccumulate("uncache_memBackTypeMM", io.lsq.req.fire && io.lsq.req.bits.memBackTypeMM)
  XSPerfAccumulate("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc)
  XSPerfAccumulate("uncache_outstanding", uState =/= s_refill_req && mem_acquire.fire)
  XSPerfAccumulate("forward_count", PopCount(io.forward.map(_.forwardMask.asUInt.orR)))
  XSPerfAccumulate("forward_vaddr_match_failed", PopCount(f1_tagMismatchVec))

  val perfEvents = Seq(
    ("uncache_mmio_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_mmio_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && !io.lsq.req.bits.nc),
    ("uncache_nc_store", io.lsq.req.fire && isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_nc_load", io.lsq.req.fire && !isStore(io.lsq.req.bits.cmd) && io.lsq.req.bits.nc),
    ("uncache_outstanding", uState =/= s_refill_req && mem_acquire.fire),
    ("forward_count", PopCount(io.forward.map(_.forwardMask.asUInt.orR))),
    ("forward_vaddr_match_failed", PopCount(f1_tagMismatchVec))
  )

  generatePerfEvent()
  //  End
}
