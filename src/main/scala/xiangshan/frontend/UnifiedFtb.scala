/***************************************************************************************
 * Copyright (c) 2024 Nanjing University
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

package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import utility.{DelayN, GTimer, HoldUnless, ReplacementPolicy, SRAMTemplate}
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.cache.{L1CacheParameters, MemoryOpConstants}
import xiangshan.frontend.{FTB, FTBEntry, FTBEntryWithTag, FTBMeta, FtbSlot}

import scala.{Tuple2 => &}

trait HasUnifiedFtbParams extends FTBParams with HasBPUConst {
  val filterEntries: Int = 4096
  val filterBuckets: Int = 1024
  val filterMaxAttempts: Int = 4

  def FtbBusDataWith: Int = l1BusDataWidth
  def beatBits: Int = FtbBusDataWith
  def beatBytes: Int = beatBits / 8
  def refillCycles: Int = CacheLineSize / beatBits
  def blockBytes: Int = CacheLineSize / 8

  val readPortIdx: Int = 0
  val writePortIdx: Int = 1
}

class FTBEntryWithTagSet(implicit p: Parameters) extends FTBEntryWithTag with FTBParams with BPUUtils {
  require(log2Ceil(numSets) == 9, "Other values are not considered")
  val setIdx: UInt = UInt(log2Ceil(numSets).W) // 9
}

object FTBEntryWithTagSet {
  def apply(tag: UInt, setIdx: UInt, entry: FTBEntry)(implicit p: Parameters): FTBEntryWithTagSet = {
    val e = Wire(new FTBEntryWithTagSet)
    e.entry := entry
    e.tag := tag
    e.setIdx := setIdx
    e
  }
}

class CuckooFilter(val numEntries: Int, val numBuckets: Int, val maxAttempts: Int)
                  (implicit p: Parameters) extends XSModule {
  // Assert parameters for robustness
  require(numEntries > 0, "Number of entries must be greater than 0.")
  require(numBuckets > 0, "Number of buckets must be greater than 0.")
  require(isPow2(numBuckets), "Number of buckets must be a power of 2.")
  require(numEntries % numBuckets == 0, "Number of entries must be divisible by number of buckets.")
  def filterOffset: Int = log2Ceil(CacheLineSize / 8)

  val entriesPerBucket: Int = numEntries / numBuckets // Compute entries per bucket
  require(isPow2(entriesPerBucket), "Entries per bucket must be a power of 2.")
  val hashBits: Int = log2Ceil(numBuckets) // Compute number of bits for hash result

  val io = IO(new Bundle {
    val keyRead: ValidIO[UInt] = Flipped(Valid(UInt(VAddrBits.W)))
    val keyWrite: ValidIO[UInt] = Flipped(Valid(UInt(VAddrBits.W)))
    val matchFound: Bool = Output(Bool())
    val reset: Bool = Input(Bool()) // Reset signal
  })
  val reducedKeyRead: UInt = io.keyRead.bits(VAddrBits - 1, filterOffset)
  val reducedKeyWrite: UInt = io.keyWrite.bits(VAddrBits - 1, filterOffset)

  class CuckooFilterEntry() extends XSBundle {
    val valid: Bool = Bool()
    val fp: UInt = UInt(hashBits.W)
  }

  // Hash functions
  def hash1(key: UInt): UInt = {
    val numParts = (VAddrBits + hashBits - 1) / hashBits // Calculate number of parts
    val paddedKey = key.pad(hashBits * numParts)
    // Split key into parts
    val parts = VecInit.tabulate(numParts)(i => paddedKey(hashBits * (i + 1) - 1, hashBits * i))
    parts.foldLeft(0.U(hashBits.W))((acc, part) => acc ^ part)
  }

  def hash2(key: UInt): UInt = {
    val numParts = (VAddrBits + hashBits - 1) / hashBits // Calculate number of parts
    val paddedKey = Cat(key, 0.U((hashBits * numParts - key.getWidth).W))
    // Split key into parts
    val parts = VecInit.tabulate(numParts)(i => paddedKey(hashBits * (i + 1) - 1, hashBits * i))
    parts.foldLeft(0.U(hashBits.W))((acc, part) => acc ^ part)
  }

  // Memory
  val table = Reg(Vec(numBuckets, Vec(entriesPerBucket, new CuckooFilterEntry)))

  // Helper function to check if fingerprint is present in bucket
  def fpInBucket(fp: UInt, bucket: Vec[CuckooFilterEntry]): Bool = {
    bucket.map(e => e.fp === fp && e.valid).reduce(_ || _)
  }

  // Lookup
  val rReadHash1: UInt = Mux(io.keyRead.valid, hash1(reducedKeyRead), 0.U(hashBits.W))
  val rReadHash2: UInt = Mux(io.keyRead.valid, hash2(reducedKeyRead), 0.U(hashBits.W))
  val rReadFingerprint: UInt = rReadHash1 ^ rReadHash2
  val rReadBucket1: Vec[CuckooFilterEntry] = table(rReadHash1)
  val rReadBucket2: Vec[CuckooFilterEntry] = table(rReadHash2)

  io.matchFound := Mux(io.keyRead.valid, (fpInBucket(rReadFingerprint, rReadBucket1) ||
    fpInBucket(rReadFingerprint, rReadBucket2)), false.B)
//  io.matchFound := true.B

  // Write
  // before writing, read these buckets
  val wReadHash1: UInt = Wire(UInt(hashBits.W))
  val wReadHash2: UInt = Wire(UInt(hashBits.W))
  val wReadFingerprint: UInt = Wire(UInt(hashBits.W))
  val wReadBucket1: Vec[CuckooFilterEntry] = table(wReadHash1)
  val wReadBucket2: Vec[CuckooFilterEntry] = table(wReadHash2)
  val bucket1InvalidBits: IndexedSeq[Bool] = wReadBucket1.map(!_.valid)
  val bucket2InvalidBits: IndexedSeq[Bool] = wReadBucket2.map(!_.valid)
  val bucket1HasSpace: Bool = bucket1InvalidBits.reduce(_ || _)
  val bucket2HasSpace: Bool = bucket2InvalidBits.reduce(_ || _)
  val hitInBucket1: Bool = fpInBucket(wReadFingerprint, wReadBucket1)
  val hitInBucket2: Bool = fpInBucket(wReadFingerprint, wReadBucket2)
  val hitInBucket: Bool = hitInBucket1 || hitInBucket2

  // main logic of writing
  val writeEnable: Bool = Wire(Bool())
  val writeBucket: UInt = Wire(UInt(hashBits.W))
  val writeEntry: UInt = Wire(UInt(log2Ceil(entriesPerBucket).W))
  val writeFingerprint: UInt = Wire(UInt(hashBits.W))
  when (writeEnable) {
    table(writeBucket)(writeEntry).fp := writeFingerprint
    table(writeBucket)(writeEntry).valid := true.B
  }

  // LFSR's output and control
  val lfsrIncrement: Bool = Wire(Bool())
  lfsrIncrement := false.B
  val randomIndex: UInt = LFSR(log2Ceil(entriesPerBucket), lfsrIncrement)
  // When state === 0.U, there's no inflight writing
  val writeState: UInt = RegInit(0.U(log2Ceil(maxAttempts).W))
  val acceptNewWrite: Bool = writeState === 0.U
  val doingReplacement: Bool = writeState =/= 0.U

  // direct write
  val directWriteHash1: UInt = hash1(reducedKeyWrite)
  val directWriteHash2: UInt = hash2(reducedKeyWrite)
  val directWriteFingerprint: UInt = directWriteHash1 ^ directWriteHash2

  // replace
  val tmpFingerprint: UInt = Reg(UInt(hashBits.W))
  val tmpHash1: UInt = Reg(UInt(hashBits.W))
  val tmpHash2: UInt = tmpHash1 ^ tmpFingerprint

  wReadHash1       := Mux(acceptNewWrite, directWriteHash1,       tmpHash1)
  wReadHash2       := Mux(acceptNewWrite, directWriteHash2,       tmpHash2)
  wReadFingerprint := Mux(acceptNewWrite, directWriteFingerprint, tmpFingerprint)

  writeEnable := (doingReplacement && !hitInBucket2) || (io.keyWrite.valid && !hitInBucket)
  writeFingerprint := Mux(acceptNewWrite, directWriteFingerprint, tmpFingerprint)
  when (acceptNewWrite) {
    // If bucket1 has space, insert to bucket1.
    // Else insert to bucket2 (probably need replace).
    writeBucket := Mux(bucket1HasSpace, directWriteHash1, directWriteHash2)
    when (bucket1HasSpace) {
      writeEntry := PriorityEncoder(bucket1InvalidBits) // Find the first empty slot
    }.elsewhen(bucket2HasSpace) {
      writeEntry := PriorityEncoder(bucket2InvalidBits) // Find the first empty slot
    }.otherwise {
      // Both buckets are full, perform cuckoo replacement in bucket2
      writeEntry := randomIndex
    }
  }.otherwise {
    writeBucket := tmpHash2
    writeEntry := Mux(bucket2HasSpace, PriorityEncoder(bucket2InvalidBits), randomIndex)
  }

  when (acceptNewWrite) { // deal with new request
    when (io.keyWrite.valid && !hitInBucket) {
      when (!bucket1HasSpace && !bucket2HasSpace) {
        // Both buckets are full, perform cuckoo replacement
        writeState := maxAttempts.asUInt
        lfsrIncrement := true.B
        tmpFingerprint := wReadBucket2(randomIndex).fp
        tmpHash1 := directWriteHash2
      }
    }
  }.otherwise { // deal with previous request
    when (!hitInBucket2) {
      when (bucket2HasSpace) {
        // hit or find empty entry
        writeState := 0.U
      }.otherwise {
        // miss
        writeState := writeState - 1.U
        lfsrIncrement := true.B
        tmpFingerprint := wReadBucket2(randomIndex).fp
        tmpHash1 := tmpHash2
      }
    }
  }

  // Reset
  when(io.reset) {
    for (i <- 0 until numBuckets) {
      for (j <- 0 until entriesPerBucket) {
        table(i)(j).valid := false.B
      }
    }
  }
}

class UnifiedFtbReadReq(implicit p: Parameters) extends XSBundle {
  val reducedVaddr: UInt = UInt((VAddrBits - 1).W)
}

class UnifiedFtbReadResp(implicit p: Parameters) extends XSBundle {
  val denied: Bool = Bool()
  val data: UInt = UInt(CacheLineSize.W)
}

class UnifiedFtbWriteReq(implicit p: Parameters) extends XSBundle {
  val reducedVaddr: UInt = UInt((VAddrBits - 1).W)
  val data: UInt = UInt(CacheLineSize.W)
}

class UnifiedFtbPortIO(edge: TLEdgeOut)(implicit p: Parameters) extends XSBundle with HasUnifiedFtbParams {
  val hartId: UInt = Input(UInt(8.W))

  // To FTB components
  val readReq: DecoupledIO[UnifiedFtbReadReq] = Flipped(DecoupledIO(new UnifiedFtbReadReq))
  val readResp: Valid[UnifiedFtbReadResp] = ValidIO(new UnifiedFtbReadResp)
  val writeReq: DecoupledIO[UnifiedFtbWriteReq] = Flipped(DecoupledIO(new UnifiedFtbWriteReq))
}

/*
 * Connect me to Unified FTB in the FrontendImp module
 */
class UnifiedFtbPort()(implicit p: Parameters) extends LazyModule with HasUnifiedFtbParams {
  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      name = "FTB",
      sourceId = IdRange(0, 2), // 0 for prefetch engine, 1 for group generator
      supportsProbe = TransferSizes(beatBytes)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))
  lazy val module = new UnifiedFtbPortImp(this)
}

class UnifiedFtbPortImp(outer: UnifiedFtbPort) extends LazyModuleImp(outer) with HasUnifiedFtbParams {
  val (bus, edge) = outer.clientNode.out.head
  val io = IO(new UnifiedFtbPortIO(edge))
  dontTouch(bus)

  println("Instantiating Unified FTB Port")

  // Only focus on A and D channel now
  bus.b.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  class UnifiedFtbReadPort(edge: TLEdgeOut, id: Int) extends XSModule with HasUnifiedFtbParams with HasIFUConst {
    val io = IO(new Bundle{
      // To FTB components
      val readReq: DecoupledIO[UnifiedFtbReadReq] = Flipped(DecoupledIO(new UnifiedFtbReadReq))
      val readResp: Valid[UnifiedFtbReadResp] = ValidIO(new UnifiedFtbReadResp)

      // tilelink channel
      val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
      val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    })
    assert(!(io.mem_grant.valid && io.mem_grant.bits.opcode =/= 1.U), "Invalid message. Only support AccessAckData.")
    // Read state machine
    val s_idle :: s_send_mem_acquire :: s_wait_mem_grant :: s_write_back_wait_resp :: Nil = Enum(4)
    val readState = RegInit(s_idle)

    val req = Reg(new UnifiedFtbReadReq)
    val req_denied = RegInit(false.B)
    val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

    // channel A: Request messages sent to an address
    io.readReq.ready := (readState === s_idle)
    io.mem_acquire.valid := (readState === s_send_mem_acquire)
    io.mem_acquire.bits := DontCare

    // channel D: Response messages from an address
    io.mem_grant.ready := true.B
    val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
    val respDataReg = Reg(Vec(refillCycles, UInt(beatBits.W)))
    io.readResp.valid := (readState === s_write_back_wait_resp)
    io.readResp.bits.data := Cat(respDataReg.reverse).pad(CacheLineSize)
    io.readResp.bits.denied := req_denied
    //state change
    switch(readState) {
      is(s_idle) {
        when(io.readReq.fire) {
          readBeatCnt := 0.U
          readState := s_send_mem_acquire
          req.reducedVaddr := io.readReq.bits.reducedVaddr
        }
      }

      // memory request
      is(s_send_mem_acquire) {
        when(io.mem_acquire.fire) {
          readState := s_wait_mem_grant
        }
      }

      is(s_wait_mem_grant) {
        when(edge.hasData(io.mem_grant.bits)) {
          when(io.mem_grant.fire) {
            readBeatCnt := readBeatCnt + 1.U
            respDataReg(readBeatCnt) := io.mem_grant.bits.data
            req_denied := io.mem_grant.bits.denied
            assert(io.mem_grant.bits.corrupt === 0.U)
            when(readBeatCnt === (refillCycles - 1).U) {
              assert(refill_done, "refill not done!")
              readState := s_write_back_wait_resp
            }
          }
        }
      }

      is(s_write_back_wait_resp) {
        when(io.readResp.fire) {
          readState := s_idle
        }
      }
    }

    val getBlock = edge.Get(
      fromSource = id.U,
      toAddress = addrAlign(req.reducedVaddr, blockBytes, PAddrBits),
      lgSize = (log2Up(blockBytes)).U
    )._2

    io.mem_acquire.bits := getBlock
  }

  class UnifiedFtbWritePort(edge: TLEdgeOut, id: Int) extends XSModule with HasUnifiedFtbParams with HasIFUConst {
    val io = IO(new Bundle{
      // To FTB components
      val writeReq: DecoupledIO[UnifiedFtbWriteReq] = Flipped(DecoupledIO(new UnifiedFtbWriteReq))

      // tilelink channel
      val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
      val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    })
    dontTouch(io)
    assert(!(io.mem_grant.valid && io.mem_grant.bits.opcode =/= 6.U), "Invalid message. Only support ReleaseAck.")

    // Write state machine
    val s_idle :: s_send_mem_release :: Nil = Enum(2)
    val writeState = RegInit(s_idle)

    val req = Reg(new UnifiedFtbWriteReq)

    io.writeReq.ready := (writeState === s_idle)

    io.mem_release.valid := (writeState === s_send_mem_release)
    io.mem_release.bits := DontCare
    val writeData = Wire(Vec(refillCycles, UInt(beatBits.W)))
    for (i <- 0 until refillCycles) {
      writeData(i) := req.data(beatBits * i + beatBits - 1, beatBits * i)
    }
    val writeBeatCnt = Reg(UInt(log2Up(refillCycles).W))

    switch(writeState) {
      is(s_idle) {
        when(io.writeReq.fire) {
          writeBeatCnt := 0.U
          writeState := s_send_mem_release
          req := io.writeReq.bits
        }
      }

      is(s_send_mem_release) {
        when(io.mem_release.fire) {
          writeBeatCnt := writeBeatCnt + 1.U
          when(writeBeatCnt === (refillCycles - 1).U) {
            writeState := s_idle
          }
        }
      }
    }

    val putBlock = edge.Release(
      fromSource = id.U,
      toAddress = addrAlign(req.reducedVaddr, blockBytes, PAddrBits),
      lgSize = (log2Up(blockBytes)).U,
      shrinkPermissions = TLPermissions.NtoN,
      data = writeData(writeBeatCnt)
    )._2

    io.mem_release.bits := putBlock
    io.mem_grant.ready := true.B
    // Ignore data from mem_grant
  }

  val readPort = Module(new UnifiedFtbReadPort(edge, readPortIdx))
  readPort.io.readReq <> io.readReq
  readPort.io.readResp <> io.readResp
  readPort.io.mem_grant.valid := false.B
  readPort.io.mem_grant.bits := DontCare
  when (bus.d.bits.source === 0.U) {
    readPort.io.mem_grant <> bus.d
  }

  val writePort = Module(new UnifiedFtbWritePort(edge, writePortIdx))
  writePort.io.writeReq <> io.writeReq
  writePort.io.mem_grant.valid := false.B
  writePort.io.mem_grant.bits := DontCare
  when (bus.d.bits.source === 1.U) {
    writePort.io.mem_grant <> bus.d
  }

//  TLArbiter(TLArbiter.highestIndexFirst)(
//    bus.a, (0.U, readPort.io.mem_acquire), (refillCycles.U, writePort.io.mem_release))
  bus.a <> readPort.io.mem_acquire
  bus.c <> writePort.io.mem_release
  XSPerfAccumulate("Unified_FTB_acquire_cnt", readPort.io.mem_acquire.fire)
  XSPerfAccumulate("Unified_FTB_put_cnt", writePort.io.mem_release.fire)
}

class UnifiedFtbIO(implicit p: Parameters) extends BasePredictorIO {
  val readReq: DecoupledIO[UnifiedFtbReadReq] = DecoupledIO(new UnifiedFtbReadReq)
  val readResp: ValidIO[UnifiedFtbReadResp] = Flipped(ValidIO(new UnifiedFtbReadResp))
  val writeReq: DecoupledIO[UnifiedFtbWriteReq] = DecoupledIO(new UnifiedFtbWriteReq)

  val prefetchCtrler: Bool = Input(Bool())
  val generateCtrler: Bool = Input(Bool())
}

class UnifiedFtb(implicit p: Parameters) extends FTB with HasUnifiedFtbParams {
  override lazy val io = IO(new UnifiedFtbIO)
  dontTouch(io)

  val filter: CuckooFilter = Module(new CuckooFilter(filterEntries, filterBuckets, filterMaxAttempts))

  class PrefetchEngine(val bufSize: Int, val pfTrigger: Int) extends XSModule {
    val io = IO(new Bundle {
      val valve: Bool = Input(Bool())

      // s0
      val s0_reqPc: DecoupledIO[UInt] = Flipped(DecoupledIO(UInt(VAddrBits.W)))

      // s1
      val s1_readResp: FTBEntry = Output(new FTBEntry)
      val s1_readHit: Bool = Output(Bool())
      val s1_mainFtbHit: Bool = Input(Bool())
      val s1_filterReq: ValidIO[UInt] = Valid(UInt(VAddrBits.W))
      val s1_filterResp: Bool = Input(Bool())

      // s2: to Unified Cache
      val s2_cacheReq: DecoupledIO[UnifiedFtbReadReq] = DecoupledIO(new UnifiedFtbReadReq)
      val s2_cacheResp: ValidIO[UnifiedFtbReadResp] = Flipped(ValidIO(new UnifiedFtbReadResp))
    })
    dontTouch(io)

    // LoopFifo with synchronized read and write
    class LoopFifo(depth: Int)(implicit p: Parameters) extends Module {
      // Ensure depth is a power of 2
      require(isPow2(depth), "Depth must be a power of 2")
      class UnifiedFtbFifoEntry(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
        def entryCnt: Int = {
          CacheLineSize / (new FTBEntryWithTagSet).getWidth
        }

        val entries: Vec[FTBEntryWithTagSet] = Vec(entryCnt, new FTBEntryWithTagSet)
      }

      def entryCnt: Int = {
        CacheLineSize / (new FTBEntryWithTagSet).getWidth
      }

      val io = IO(new Bundle {
        val dataIn: ValidIO[UnifiedFtbFifoEntry] = Flipped(Valid(new UnifiedFtbFifoEntry))

        val queryPc: DecoupledIO[UInt] = Flipped(DecoupledIO(UInt(VAddrBits.W)))

        // queryResult.valid is true when the queryPc hits in the fifo.
        // queryResult.bits is the FTBEntry if queryResult.valid is true.
        val queryResult: ValidIO[FTBEntry] = Valid(new FTBEntry)
      })
      val dataInDebug = Wire(Vec(entryCnt, UInt((new FTBEntryWithTagSet).getWidth.W)))
      for (i <- 0 until entryCnt) {
        dataInDebug(i) := io.dataIn.bits.entries(i).asUInt
      }
      dontTouch(dataInDebug)

      private val querySetIdx: UInt = ftbAddr.getIdx(io.queryPc.bits)
      private val queryTag: UInt = ftbAddr.getTag(io.queryPc.bits)(tagSize-1, 0)

      // Register arrays
      private val fifoEntriesValid: Vec[Bool] = RegInit(VecInit(Seq.fill(depth)(false.B)))
      private val fifoEntries: Vec[UnifiedFtbFifoEntry] = Reg(Vec(depth, new UnifiedFtbFifoEntry))

      // Write pointer for enqueue operation
      private val writePtr: UInt = RegInit(0.U(log2Ceil(depth).W))

      // Set query interface ready signal
      io.queryPc.ready := true.B

      // Enqueue operation
      when(io.dataIn.valid) {
        fifoEntries(writePtr) := io.dataIn.bits
        fifoEntriesValid(writePtr) := true.B
        writePtr := writePtr + 1.U
      }

      // Query operation
      val queryResultEntry = Reg(new FTBEntry())
      val queryResultValid = RegInit(false.B)
      io.queryResult.valid := queryResultValid
      io.queryResult.bits := queryResultEntry

      when(io.queryPc.fire) {
        // Wire for parallel matching results
        val matchingResultVec = Wire(Vec(depth, Vec((new UnifiedFtbFifoEntry).entryCnt, Bool())))
        // Wire for matched FTB entry in each FIFO entry
        val matchingFtbEntryVec = Wire(Vec(depth, new FTBEntry))
        // Compute matching results and matching entries for each FIFO entry
        val matchingFifoEntries = for (i <- 0 until depth) yield {
          val fifoEntry = fifoEntries(i)
          matchingResultVec(i) := VecInit(fifoEntry.entries.map(e => e.tag === queryTag && e.setIdx === querySetIdx))
          matchingFtbEntryVec(i) := Mux1H(matchingResultVec(i), fifoEntry.entries.map(_.entry))
          fifoEntriesValid(i) && matchingResultVec(i).reduce(_ || _)
        }
        // Select the matching FIFO entry index
        val matchingFifoIndex = PriorityEncoder(matchingFifoEntries)

        queryResultValid := matchingFifoEntries.reduce(_ || _)
        queryResultEntry := matchingFtbEntryVec(matchingFifoIndex)
      }.otherwise {
        queryResultValid := false.B
      }


    }

    val fifo = Module(new LoopFifo(bufSize))
    fifo.io.queryPc <> io.s0_reqPc

    /************* s1 *************/
    val s1_req_valid = RegNext(io.s0_reqPc.valid)
    val s1_pred_rdata: ValidIO[FTBEntry] = HoldUnless(fifo.io.queryResult, s1_req_valid)
    io.s1_readResp := s1_pred_rdata.bits
    io.s1_readHit := s1_pred_rdata.valid

    val s1_fifoMiss: Bool = Wire(Bool())
    s1_fifoMiss := !fifo.io.queryResult.valid && s1_req_valid
    val s1_vaddr: UInt = RegNext(io.s0_reqPc.bits(VAddrBits - 1, 1))
    // Look up in the Filter
    io.s1_filterReq.valid := io.valve && s1_fifoMiss && !io.s1_mainFtbHit
    io.s1_filterReq.bits := s1_vaddr

    /************* s2 *************/
    // Send request to Unified cache
    val inflightVaddr: UInt = Reg(UInt(VAddrBits.W))

    val s_idle :: s_send_mem_acquire :: s_wait_mem_grant :: Nil = Enum(3)
    val state: UInt = RegInit(s_idle)

    io.s2_cacheReq.valid := (state === s_send_mem_acquire)
    io.s2_cacheReq.bits.reducedVaddr := inflightVaddr

    fifo.io.dataIn.valid := io.s2_cacheResp.fire && !io.s2_cacheResp.bits.denied && state === s_wait_mem_grant
    fifo.io.dataIn.bits.entries := VecInit.tabulate(fifo.io.dataIn.bits.entryCnt)(i => io.s2_cacheResp.bits.data(
      (new FTBEntryWithTagSet).getWidth * (i + 1) - 1, (new FTBEntryWithTagSet).getWidth * i
    ).asTypeOf(new FTBEntryWithTagSet)).reverse

    switch(state) {
      is(s_idle) {
        when(io.s1_filterReq.valid && io.s1_filterResp && io.valve) {
          state := s_send_mem_acquire
          inflightVaddr := s1_vaddr
        }
      }
      is(s_send_mem_acquire) {
        when(io.s2_cacheReq.fire) {
          state := s_wait_mem_grant
        }
      }
      is(s_wait_mem_grant) {
        when(io.s2_cacheResp.fire) {
          state := s_idle
        }
      }
    }
  }

  class TemporalGroupGen(val pfTrigger: Int) extends XSModule {
    val bufferSize: Int = CacheLineSize / (new FTBEntryWithTagSet).getWidth

    val io = IO(new Bundle {
      val valve: Bool = Input(Bool())

      val updatePc: UInt = Input(UInt(VAddrBits.W))
      val updateFtbEntry: ValidIO[FTBEntry] = Flipped(Valid(new FTBEntry))

      val filterWrite: ValidIO[UInt] = Valid(UInt(VAddrBits.W))
      val cacheReq: DecoupledIO[UnifiedFtbWriteReq] = DecoupledIO(new UnifiedFtbWriteReq)
    })

    val entriesBuf: Vec[FTBEntryWithTagSet] = Reg(Vec(bufferSize, new FTBEntryWithTagSet))
    val entriesDebug = Wire(Vec(bufferSize, UInt((new FTBEntryWithTagSet).getWidth.W)))
    for (i <- 0 until bufferSize) {
      entriesDebug(i) := entriesBuf(i).asUInt
    }
    dontTouch(entriesDebug)

    val updateTag: UInt = ftbAddr.getTag(io.updatePc)
    val updateSetIdx: UInt = ftbAddr.getIdx(io.updatePc)
    val updateFtbEntryWithTagSet: FTBEntryWithTagSet = FTBEntryWithTagSet(updateTag, updateSetIdx,
      io.updateFtbEntry.bits)

    val writePtr: UInt = RegInit(0.U(3.W))
    val full: Bool = writePtr === bufferSize.asUInt
    val redundant: Bool = entriesBuf.map(e => e.tag === updateTag &&
      e.setIdx === updateSetIdx && e.entry.valid).reduce(_ || _)

    val lastEntryAddr: UInt = RegEnable(io.updatePc(VAddrBits - 1, 1), io.updateFtbEntry.valid && !full && !redundant)
    //val lastEntryAddr: UInt = Cat(entriesBuf(bufferSize - 1).tag, entriesBuf(bufferSize - 1).setIdx)
    val lastGroupAddr: UInt = RegEnable(lastEntryAddr, io.cacheReq.fire)

    when(io.updateFtbEntry.valid && !full && !redundant) {
      // fill this entry
      entriesBuf(writePtr) := updateFtbEntryWithTagSet
      // move ptr to next entry
      writePtr := writePtr + 1.U
    }

    io.cacheReq.valid := full
    io.cacheReq.bits.data := Cat(entriesBuf.map(_.asUInt)).pad(CacheLineSize)
    io.cacheReq.bits.reducedVaddr := lastGroupAddr
    when(io.cacheReq.fire) {
      writePtr := 0.U
      for (i <- 0 until bufferSize) {
        entriesBuf(i).entry.valid := false.B
      }
    }

    io.filterWrite.valid := io.cacheReq.fire
    io.filterWrite.bits := lastGroupAddr
  }

  lazy val pfe: PrefetchEngine = Module(new PrefetchEngine(FtbPfBuf, FtbPfTrigger))
  val tgg: TemporalGroupGen = Module(new TemporalGroupGen(FtbPfTrigger))

  override lazy val s2_ftb_entry_dup = io.s1_fire.map(f => RegEnable(
      Mux(ftbBank.io.read_hits.valid, ftbBank.io.read_resp, pfe.io.s1_readResp), f))

  override lazy val s1_hit: Bool = (ftbBank.io.read_hits.valid || pfe.io.s1_readHit) && io.ctrl.btb_enable
//  // TODO: override me correctly!
//  override val writeWay: UInt = ftbBank.io.read_hits.bits

  for (full_pred & s2_ftb_entry & s2_pc & s1_pc & s1_fire <-
         io.out.s2.full_pred zip s2_ftb_entry_dup zip s2_pc_dup zip s1_pc_dup zip io.s1_fire) {
    full_pred.fromFtbEntry(s2_ftb_entry,
      s2_pc,
      // Previous stage meta for better timing
      Some(s1_pc, s1_fire),
      Some(Mux(ftbBank.io.update_hits.valid, ftbBank.io.read_resp, pfe.io.s1_readResp), s1_fire)
    )
  }

  io.out.last_stage_meta := RegEnable(RegEnable(FTBMeta(writeWay.asUInt, s1_hit, GTimer(),
    ftbBank.io.read_hits.valid && io.ctrl.btb_enable).asUInt, io.s1_fire(0)), io.s2_fire(0))

  // Additional lookup logic
  pfe.io.valve := io.prefetchCtrler
  pfe.io.s0_reqPc.valid := io.s0_fire(0)
  pfe.io.s0_reqPc.bits := s0_pc_dup(0)
  pfe.io.s1_mainFtbHit := ftbBank.io.read_hits.valid
  // to cuckoo filter
  filter.io.keyRead <> pfe.io.s1_filterReq
  pfe.io.s1_filterResp := filter.io.matchFound
  // to unified cache
  io.readReq <> pfe.io.s2_cacheReq
  pfe.io.s2_cacheResp <> io.readResp

  // Additional update logic
  tgg.io.valve := io.generateCtrler
  // tgg.io.updateFtbEntry.valid := DelayN(u_valid && !u_meta.hit, 2)
  tgg.io.updateFtbEntry.valid := DelayN(u_valid && (!u_meta.blockHit.get), 2)
  tgg.io.updatePc := ftbBank.io.update_pc
  tgg.io.updateFtbEntry.bits := ftb_write.entry

  // to cuckoo filter
  filter.io.keyWrite <> tgg.io.filterWrite
  // to unified cache
  io.writeReq <> tgg.io.cacheReq

  filter.io.reset := false.B
}