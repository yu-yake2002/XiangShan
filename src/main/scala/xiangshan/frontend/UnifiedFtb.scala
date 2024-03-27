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
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters}
import utility.{HoldUnless, ReplacementPolicy, SRAMTemplate}
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.frontend.{FTB, FTBEntry, FTBEntryWithTag, FTBMeta, FtbSlot}

import scala.{Tuple2 => &}

trait UnifiedFtbParams extends FTBParams with HasBPUConst {
  val bufSize: Int = FtbPfBuf
  val pfTrigger: Int = FtbPfTrigger
  val filterEntries: Int = 4096
  val filterBuckets: Int = 1024
  val filterMaxAttempts: Int = 4
}

class FTBEntryWithTagSet(implicit p: Parameters) extends FTBEntryWithTag with FTBParams with BPUUtils {
  assert(log2Ceil(numSets) == 9, "Other values are not considered")
  val setIdx: UInt = UInt(log2Ceil(numSets).W) // 9
}

object FTBEntryWithTagSet {
  def apply(tag: UInt, setIdx: UInt, entry: FTBEntry)(implicit  p: Parameters): FTBEntryWithTagSet = {
    val e = Wire(new FTBEntryWithTagSet)
    e.entry := entry
    e.tag := tag
    e.setIdx := setIdx
    e
  }
}

class UnifiedFtbFifoEntry(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  def entryCnt: Int = {
    CacheLineSize / (new FTBEntryWithTagSet).getWidth
  }

  val entries: Vec[FTBEntryWithTagSet] = Vec(entryCnt, new FTBEntryWithTagSet)
  val buf: UInt = Cat(entries.map(_.asUInt))
}

class CuckooFilter(val numEntries: Int, val numBuckets: Int, val maxAttempts: Int)
                  (implicit p: Parameters) extends XSModule {
  // Assert parameters for robustness
  assert(numEntries > 0, "Number of entries must be greater than 0.")
  assert(numBuckets > 0, "Number of buckets must be greater than 0.")
  assert(isPow2(numBuckets), "Number of buckets must be a power of 2.")
  assert(numEntries % numBuckets == 0, "Number of entries must be divisible by number of buckets.")

  val entriesPerBucket: Int = numEntries / numBuckets // Compute entries per bucket
  assert(isPow2(entriesPerBucket), "Entries per bucket must be a power of 2.")
  val hashBits: Int = log2Ceil(numBuckets) // Compute number of bits for hash result

  val io = IO(new Bundle {
    val keyRead: ValidIO[UInt] = Flipped(Valid(UInt(VAddrBits.W)))
    val keyWrite: ValidIO[UInt] = Flipped(Valid(UInt(VAddrBits.W)))
    val matchFound: Bool = Output(Bool())
    val reset: Bool = Input(Bool()) // Reset signal
  })

  class CuckooFilterEntry() extends XSBundle {
    val valid: Bool = Bool()
    val fp: UInt = UInt(hashBits.W)
  }

  // Hash functions
  def hash1(key: UInt): UInt = {
    val numParts = (VAddrBits + hashBits - 1) / hashBits // Calculate number of parts
    val paddedKey = key.pad(hashBits)
    // Split key into parts
    val parts = VecInit.tabulate(numParts)(i => paddedKey(hashBits * (i + 1) - 1, hashBits * i))
    parts.foldLeft(0.U(hashBits.W))((acc, part) => acc ^ part)
  }

  def hash2(key: UInt): UInt = {
    val numParts = (VAddrBits + hashBits - 1) / hashBits // Calculate number of parts
    val paddedKey = Cat(0.U((hashBits - key.getWidth).W), key)
    // Split key into parts
    val parts = VecInit.tabulate(numParts)(i => paddedKey(hashBits * (i + 1) - 1, hashBits * i))
    parts.foldLeft(0.U(hashBits.W))((acc, part) => acc ^ part)
  }

  // Memory
  val table: Mem[Vec[CuckooFilterEntry]] = Mem(numBuckets, Vec(entriesPerBucket, new CuckooFilterEntry))

  // Helper function to check if fingerprint is present in bucket
  def fpInBucket(fp: UInt, bucket: Vec[CuckooFilterEntry]): Bool = {
    bucket.map(e => e.fp === fp && e.valid).reduce(_ || _)
  }

  // Lookup
  val rReadHash1: UInt = Mux(io.keyRead.valid, hash1(io.keyRead.bits), 0.U(hashBits.W))
  val rReadHash2: UInt = Mux(io.keyRead.valid, hash2(io.keyRead.bits), 0.U(hashBits.W))
  val rReadFingerprint: UInt = rReadHash1 ^ rReadHash2
  val rReadBucket1: Vec[CuckooFilterEntry] = table(rReadHash1)
  val rReadBucket2: Vec[CuckooFilterEntry] = table(rReadHash2)

  io.matchFound := Mux(io.keyRead.valid, (fpInBucket(rReadFingerprint, rReadBucket1) ||
    fpInBucket(rReadFingerprint, rReadBucket2)), false.B)

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
  val directWriteHash1: UInt = hash1(io.keyWrite.bits)
  val directWriteHash2: UInt = hash2(io.keyWrite.bits)
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
        table(i)(j) := 0.U
      }
    }
  }
}

class UnifiedFtbIO(implicit p: Parameters) extends XSBundle with UnifiedFtbParams {
  val hartId = Input(UInt(8.W))
  // TODO: Add more signals
}

/*
 * Connect me to Unified FTB in the FrontendImp module
 */
class UnifiedFtbPort()(implicit p: Parameters) extends LazyModule {
  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      name = "FTB",
      sourceId = IdRange(0, 2),
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))
  lazy val module = new UnifiedFtbPortImp(this)
}

class UnifiedFtbPortImp(outer: UnifiedFtbPort) extends LazyModuleImp(outer) {
  println("Instantiating Unified FTB Port")
}

class UnifiedFtb(implicit p: Parameters) extends FTB with UnifiedFtbParams {
  val ioUnifiedCache = IO(new Bundle {
    val prefetchCtrler: Bool = Input(Bool())
    val generateCtrler: Bool = Input(Bool())
  })

  /************ Tilelink ************/
  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      name = "FTB",
      sourceId = IdRange(0, 2)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))


  val filter: CuckooFilter = Module(new CuckooFilter(filterEntries, filterBuckets, filterMaxAttempts))

  class PrefetchEngine(val bufSize: Int, val pfTrigger: Int) extends XSModule {
    val io = IO(new Bundle {
      val valve: Bool = Input(Bool())
      val reqPc: DecoupledIO[UInt] = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val readResp: FTBEntry = Output(new FTBEntry)
      val readHit: Bool = Output(Bool())

      // Cuckoo Filter
      val filterReq: ValidIO[UInt] = Valid(UInt(VAddrBits.W))
      val filterResp: Bool = Input(Bool())
      // TODO: data path to L2 cache
    })

    class LoopFifo(depth: Int)(implicit p: Parameters) extends Module {
      // Ensure depth is a power of 2
      assert(isPow2(depth), "Depth must be a power of 2")

      val io = IO(new Bundle {
        val enq: Bool = Input(Bool())
        val dataIn: UnifiedFtbFifoEntry = Input(new UnifiedFtbFifoEntry)

        val queryPc: DecoupledIO[UInt] = Flipped(DecoupledIO(UInt(VAddrBits.W)))
        val queryResult: ValidIO[FTBEntry] = Valid(new FTBEntry)
      })
      private val querySetIdx: UInt = ftbAddr.getIdx(io.queryPc.bits)
      private val queryTag: UInt = ftbAddr.getTag(io.queryPc.bits)(tagSize-1, 0)

      // Register arrays
      private val fifoEntriesValid: Vec[Bool] = RegInit(VecInit(Seq.fill(depth)(false.B)))
      private val fifoEntries: Vec[UnifiedFtbFifoEntry] = Reg(Vec(depth, new UnifiedFtbFifoEntry))

      // Write pointer for enqueue operation
      private val writePtr: UInt = RegInit(0.U(log2Ceil(depth).W))

      // Set query interface ready signal
      io.queryPc.ready := true.B
      // Initialize query result to invalid
      io.queryResult.valid := false.B
      io.queryResult.bits := DontCare

      // Enqueue operation
      when(io.enq) {
        fifoEntries(writePtr) := io.dataIn
        fifoEntriesValid(writePtr) := true.B
        writePtr := writePtr + 1.U
      }

      // Query operation
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
        val matchingFifoIndex = PriorityEncoder(matchingFifoEntries.reverse)

        io.queryResult.valid := matchingFifoEntries.reduce(_ || _)
        io.queryResult.bits := matchingFtbEntryVec(matchingFifoIndex)
      }
    }

    val fifo = new LoopFifo(bufSize)

    val pred_rdata: ValidIO[FTBEntry] = HoldUnless(fifo.io.queryResult, RegNext(io.reqPc.valid))

    fifo.io.queryPc := io.reqPc
    io.reqPc.ready := true.B
    val req_tag: UInt = RegEnable(ftbAddr.getTag(io.reqPc.bits)(tagSize-1, 0), io.reqPc.valid)
    val req_idx: UInt = RegEnable(ftbAddr.getIdx(io.reqPc.bits), io.reqPc.valid)
    io.readResp := pred_rdata.bits
    io.readHit := pred_rdata.valid

    val fifoMiss: Bool = !fifo.io.queryResult.valid && fifo.io.queryPc.valid

    /************* s2 *************/
    // Look up in the Filter
    io.filterReq.valid := RegNext(io.valve && fifoMiss)
    io.filterReq.bits := RegNext(io.reqPc)

    /************* s3 *************/
    // Send request to Unified cache
    val filterHit: Bool = RegNext(io.filterReq.valid && io.filterResp)

    when (filterHit && io.valve) {
      // TODO: No available entry, send request

    }
  }

  class TemporalGroupGen(val bufSize: Int, val pfTrigger: Int) extends XSModule {
    val bufferSize: Int = CacheLineSize / (new FTBEntryWithTagSet).getWidth

    val io = IO(new Bundle {
      val valve: Bool = Input(Bool())

      val updatePc: UInt = Input(UInt(VAddrBits.W))
      val updateFtbEntry: ValidIO[FTBEntryWithTag] = Flipped(Valid(new FTBEntryWithTag))

      val filterWrite: ValidIO[UInt] = Valid(UInt(VAddrBits.W))

      // TODO: data path to Unified cache
      val unifiedCacheReq = DecoupledIO(UInt(CacheLineSize.W))
    })

    val entriesBuf: Vec[FTBEntryWithTagSet] = Reg(Vec(bufferSize, new FTBEntryWithTagSet))
    val lastEntryAddr: UInt = Cat(entriesBuf(bufferSize - 1).tag, entriesBuf(bufferSize - 1).setIdx)
    val lastAddr = RegEnable(lastEntryAddr, io.unifiedCacheReq.fire)

    val updateTag: UInt = io.updateFtbEntry.bits.tag
    val updateSetIdx: UInt = ftbAddr.getIdx(io.updatePc)
    val updateFtbEntryWithTagSet: FTBEntryWithTagSet = FTBEntryWithTagSet(updateTag, updateSetIdx,
      io.updateFtbEntry.bits.entry)

    val writePtr: UInt = RegInit(0.U(3.W))
    val full: Bool = writePtr === bufferSize.asUInt
    val redundant: Bool = entriesBuf.map(e => e.tag === updateTag &&
      e.setIdx === updateSetIdx && e.entry.valid).reduce(_ || _)

    when(io.updateFtbEntry.valid && !full && !redundant) {
      // fill this entry
      entriesBuf(writePtr) := updateFtbEntryWithTagSet
      // move ptr to next entry
      writePtr := writePtr + 1.U
    }

    io.unifiedCacheReq.valid := full
    io.unifiedCacheReq.bits := Cat(entriesBuf.map(_.asUInt)).pad(CacheLineSize)
    when(io.unifiedCacheReq.fire) {
      writePtr := 0.U
      for (i <- 0 until bufferSize) {
        entriesBuf(i).entry.valid := false.B
      }
    }

  }

  val pfe: PrefetchEngine = Module(new PrefetchEngine(bufSize, pfTrigger))
  val tgg: TemporalGroupGen = Module(new TemporalGroupGen(bufSize, pfTrigger))

  override val s2_ftb_entry_dup = io.s1_fire.map(f => RegEnable(
    Mux(ftbBank.io.read_hits.valid, ftbBank.io.read_resp, pfe.io.readResp), f))

  override val s1_hit: Bool = (ftbBank.io.read_hits.valid || pfe.io.readHit) && io.ctrl.btb_enable
  // TODO: override me correctly!
  override val writeWay: UInt = ftbBank.io.read_hits.bits

  for (full_pred & s2_ftb_entry & s2_pc & s1_pc & s1_fire <-
         io.out.s2.full_pred zip s2_ftb_entry_dup zip s2_pc_dup zip s1_pc_dup zip io.s1_fire) {
    full_pred.fromFtbEntry(s2_ftb_entry,
      s2_pc,
      // Previous stage meta for better timing
      Some(s1_pc, s1_fire),
      Some(Mux(ftbBank.io.update_hits.valid, ftbBank.io.read_resp, pfe.io.readResp), s1_fire)
    )
  }

  // Additional lookup logic
  pfe.io.valve := ioUnifiedCache.prefetchCtrler
  pfe.io.reqPc.valid := io.s0_fire(0)
  pfe.io.reqPc.bits := s0_pc_dup(0)
  // TODO: use readResp
  filter.io.keyRead := pfe.io.filterReq
  pfe.io.filterResp := filter.io.matchFound
  // TODO: connect pfe to Unified Cache

  // Additional update logic
  tgg.io.valve := ioUnifiedCache.generateCtrler
  tgg.io.updatePc := update.pc
  tgg.io.updateFtbEntry.valid := io.update.valid
  tgg.io.updateFtbEntry.bits := update.ftb_entry
  filter.io.keyWrite := tgg.io.filterWrite
  // TODO: connect tgg to Unified Cache
  // := tgg.io.unifiedCacheReq
}