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

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.issue.SchdBlockParams
import xiangshan.backend.datapath.{DataSource}
import xiangshan.backend.fu.matrix.Bundles.Mtilex

class BusyTableReadIO(implicit p: Parameters) extends XSBundle {
  val req = Input(UInt(PhyRegIdxWidth.W))
  val resp = Output(Bool())
  val loadDependency = Vec(LoadPipelineWidth, Output(UInt(LoadDependencyWidth.W)))
}


class VlBusyTableReadIO(implicit p: Parameters) extends XSBundle {
  val is_nonzero = Output(Bool())
  val is_vlmax = Output(Bool())
}

class MxBusyTableReadIO(implicit p: Parameters) extends XSBundle {
  val is_zero = Output(Bool())
  val is_mxmax = Output(Bool())
}

class BusyTable(numReadPorts: Int, numWritePorts: Int, numPhyPregs: Int, pregWB: PregWB)(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // set preg state to busy
    val allocPregs = Vec(RenameWidth, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // set preg state to ready (write back regfile + rob walk)
    val wbPregs = Vec(numWritePorts, Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    // fast wakeup
    val wakeUpInt = Flipped(backendParams.intSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpFp = Flipped(backendParams.fpSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpVec = Flipped(backendParams.vfSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpMf = Flipped(backendParams.mfSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpMem = Flipped(backendParams.memSchdParams.get.genIQWakeUpOutValidBundle)
    // cancelFromDatapath
    val og0Cancel = Input(ExuVec())
    // cancelFromMem
    val ldCancel = Vec(backendParams.LdWakeupCnt, Flipped(new LoadCancelIO))
    // read preg state
    val read = Vec(numReadPorts, new BusyTableReadIO)
  })

  val allExuParams = backendParams.allExuParams
  val intBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeIntRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readIntRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val fpBusyTableNeedLoadCancel = false
  val vfBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeVfRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVecRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val v0BusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeV0Rf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVecRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val vlBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeVlRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readVlRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val mxBusyTableNeedLoadCancel = allExuParams.map(x =>
    x.needLoadDependency && x.writeMxRf && x.iqWakeUpSourcePairs.map(y => y.sink.getExuParam(allExuParams).readMxRf).foldLeft(false)(_ || _)
  ).reduce(_ || _)
  val needLoadCancel = pregWB match {
    case IntWB(_, _) => intBusyTableNeedLoadCancel
    case FpWB(_, _) => fpBusyTableNeedLoadCancel
    case VfWB(_, _) => vfBusyTableNeedLoadCancel
    case V0WB(_, _) => v0BusyTableNeedLoadCancel
    case VlWB(_, _) => vlBusyTableNeedLoadCancel
    case MxWB(_, _) => mxBusyTableNeedLoadCancel
    case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
  }
  if (!needLoadCancel) println(s"[BusyTable]: WbConfig ${pregWB} busyTable don't need loadCancel")
  val loadCancel = if (needLoadCancel) io.ldCancel else 0.U.asTypeOf(io.ldCancel)
  val allWakeUp = io.wakeUpInt ++ io.wakeUpFp ++ io.wakeUpVec ++ io.wakeUpMf ++ io.wakeUpMem
  val wakeUpIn = pregWB match {
    case IntWB(_, _) => allWakeUp.filter{x => x.bits.params.writeIntRf && (x.bits.params.hasLoadExu || x.bits.params.hasAluFu)}
    case FpWB(_, _) => allWakeUp.filter{x => x.bits.params.writeFpRf && !x.bits.params.hasLoadExu}
    case VfWB(_, _) => allWakeUp.filter(_.bits.params.writeVfRf)
    case V0WB(_, _) => allWakeUp.filter(_.bits.params.writeV0Rf)
    case VlWB(_, _) => allWakeUp.filter(_.bits.params.writeVlRf)
    case MxWB(_, _) => allWakeUp.filter(_.bits.params.writeMxRf)
    case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
  }
  val loadDependency = RegInit(0.U.asTypeOf(Vec(numPhyPregs, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  val shiftLoadDependency = Wire(Vec(wakeUpIn.size, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
  val tableUpdate = Wire(Vec(numPhyPregs, Bool()))
  val wakeupOHVec = Wire(Vec(numPhyPregs, UInt(wakeUpIn.size.W)))

  def reqVecToMask(rVec: Vec[Valid[UInt]]): UInt = {
    ParallelOR(rVec.map(v => Mux(v.valid, UIntToOH(v.bits), 0.U)))
  }

  shiftLoadDependency.zip(wakeUpIn).map{ case (deps, wakeup) =>
    if (wakeup.bits.params.hasLoadExu) {
      deps.zipWithIndex.map{ case (dep, i) =>
        if (backendParams.getLdExuIdx(wakeup.bits.params) == i) dep := 1.U
        else dep := 0.U
      }
    }
    else {
      deps.zip(wakeup.bits.loadDependency).map{ case (sink, source) =>
        sink := source << 1
      }
    }
  }

  wakeupOHVec.zipWithIndex.foreach{ case (wakeupOH, idx) =>
    val tmp = pregWB match {
      case IntWB(_, _) => wakeUpIn.map(x => x.valid && x.bits.rfWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case FpWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.fpWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case VfWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.vecWen && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case V0WB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.v0Wen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case VlWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.vlWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case MxWB(_, _)  => wakeUpIn.map(x => x.valid && x.bits.mxWen  && UIntToOH(x.bits.pdest)(idx) && !LoadShouldCancel(Some(x.bits.loadDependency), loadCancel) && !(x.bits.is0Lat && io.og0Cancel(x.bits.params.exuIdx)))
      case _ => throw new IllegalArgumentException(s"WbConfig ${pregWB} is not permitted")
    }
    wakeupOH := (if (wakeUpIn.nonEmpty) VecInit(tmp.toSeq).asUInt else 0.U)
  }
  val wbMask = reqVecToMask(io.wbPregs)
  val allocMask = reqVecToMask(io.allocPregs)
  val wakeUpMask = VecInit(wakeupOHVec.map(_.orR).toSeq).asUInt
  val ldCancelMask = loadDependency.map(x => LoadShouldCancel(Some(x), loadCancel))

  loadDependency.zipWithIndex.foreach{ case (ldDp, idx) =>
    when(wakeUpMask(idx)) {
      ldDp := (if (wakeUpIn.nonEmpty) Mux1H(wakeupOHVec(idx), shiftLoadDependency) else 0.U.asTypeOf(ldDp))
    }
      .elsewhen(ldDp.map(x => x.orR).reduce(_ | _)) {
        ldDp := VecInit(ldDp.map(x => x << 1))
      }
  }

  /*
  we can ensure that the following conditions are mutually exclusive
  wakeUp and cancel (same pdest) may arrive at the same cycle
  for a pdest:
    rename alloc => wakeUp / cancel => ... => wakeUp / cancel => wakeUp
  or
    rename alloc => wbMask  //TODO we still need wbMask because wakeUp signal is partial now
  in wakeUpMask, we filter ogCancel and loadTransCancel at the same cycle
   */
  val table = VecInit((0 until numPhyPregs).zip(tableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || wakeUpMask(idx) || wbMask(idx))
  }).asUInt

  tableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(wakeUpMask(idx) || wbMask(idx)) {
      update := false.B                                   //ready
    }
      .elsewhen(allocMask(idx) || ldCancelMask(idx)) {
        update := true.B                                    //busy
        if (idx == 0 && pregWB.isInstanceOf[IntWB]) {
          // Int RegFile 0 is always ready
          update := false.B
        }
      }
      .otherwise {
        update := table(idx)
      }
  }

  io.read.foreach{ case res =>
    val readBypass = VecInit(io.allocPregs.map(x => x.valid && x.bits === res.req))
    res.resp := !(table(res.req) || readBypass.asUInt.orR)
    res.loadDependency := (if (needLoadCancel) loadDependency(res.req) else 0.U.asTypeOf(res.loadDependency))
  }

  val oddTable = table.asBools.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
  val evenTable = table.asBools.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
  val busyCount = RegNext(RegNext(PopCount(oddTable)) + RegNext(PopCount(evenTable)))

  XSPerfAccumulate("busy_count", PopCount(table))

  val perfEvents = Seq(
    ("bt_std_freelist_1_4_valid", busyCount < (numPhyPregs / 4).U                                        ),
    ("bt_std_freelist_2_4_valid", busyCount > (numPhyPregs / 4).U && busyCount <= (numPhyPregs / 2).U    ),
    ("bt_std_freelist_3_4_valid", busyCount > (numPhyPregs / 2).U && busyCount <= (numPhyPregs * 3 / 4).U),
    ("bt_std_freelist_4_4_valid", busyCount > (numPhyPregs * 3 / 4).U                                    )
  )
  generatePerfEvent()

}

class VlBusyTable(numReadPorts: Int, numWritePorts: Int, numPhyPregs: Int, pregWB: PregWB)(implicit p: Parameters) extends BusyTable(numReadPorts, numWritePorts, numPhyPregs, pregWB) {

  val io_vl_Wb = IO(new Bundle() {
    val vlWriteBackInfo = new Bundle {
      val vlFromIntIsZero  = Input(Bool())
      val vlFromIntIsVlmax = Input(Bool())
      val vlFromVfIsZero   = Input(Bool())
      val vlFromVfIsVlmax  = Input(Bool())
    }
  })
  val io_vl_read = IO(new Bundle() {
    val vlReadInfo = Vec(numReadPorts, new VlBusyTableReadIO)
  })

  var intSchdVlWbPort = p(XSCoreParamsKey).intSchdVlWbPort
  var vfSchdVlWbPort = p(XSCoreParamsKey).vfSchdVlWbPort

  val nonzeroTableUpdate = Wire(Vec(numPhyPregs, Bool()))
  val vlmaxTableUpdate = Wire(Vec(numPhyPregs, Bool()))

  val intVlWb = Mux(io.wbPregs(intSchdVlWbPort).valid, UIntToOH(io.wbPregs(intSchdVlWbPort).bits), 0.U)
  val vfVlWb = Mux(io.wbPregs(vfSchdVlWbPort).valid, UIntToOH(io.wbPregs(vfSchdVlWbPort).bits), 0.U)
  // when other ports write back, we cannot know the vl value, so we should set the vl table to busy
  val otherPortsWb = io.wbPregs.zipWithIndex.filter(x => x._2 != intSchdVlWbPort && x._2 != vfSchdVlWbPort).map(x => Mux(x._1.valid, UIntToOH(x._1.bits), 0.U)).reduce(_ | _)

  val nonzeroTable = VecInit((0 until numPhyPregs).zip(nonzeroTableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || intVlWb(idx) || vfVlWb(idx) || otherPortsWb(idx))
  }).asUInt
  val vlmaxTable = VecInit((0 until numPhyPregs).zip(vlmaxTableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || intVlWb(idx) || vfVlWb(idx) || otherPortsWb(idx))
  }).asUInt


  nonzeroTableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(intVlWb(idx)) {
      // int schd vl write back, check whether the vl is zero
      update := io_vl_Wb.vlWriteBackInfo.vlFromIntIsZero
    }.elsewhen(vfVlWb(idx)) {
      // vf schd vl write back, check whether the vl is zero
      update := io_vl_Wb.vlWriteBackInfo.vlFromVfIsZero
    }.elsewhen(otherPortsWb(idx) || allocMask(idx) || ldCancelMask(idx)) {
      update := true.B
    }.otherwise {
      update := nonzeroTable(idx)
    }
  }

  vlmaxTableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(intVlWb(idx)) {
      // int schd vl write back, check whether the vl is vlmax
      update := !io_vl_Wb.vlWriteBackInfo.vlFromIntIsVlmax
    }.elsewhen(vfVlWb(idx)) {
      // vf schd vl write back, check whether the vl is vlmax
      update := !io_vl_Wb.vlWriteBackInfo.vlFromVfIsVlmax
    }.elsewhen(otherPortsWb(idx) || allocMask(idx) || ldCancelMask(idx)) {
      update := true.B
    }.otherwise {
      update := vlmaxTable(idx)
    }
  }

  io_vl_read.vlReadInfo.zip(io.read).foreach{ case (vlRes, res) =>
    vlRes.is_nonzero := !nonzeroTable(res.req)
    vlRes.is_vlmax := !vlmaxTable(res.req)
  }
}

class MxBusyTable(numReadPorts: Int, numWritePorts: Int, numPhyPregs: Int, pregWB: PregWB)(implicit p: Parameters) extends BusyTable(numReadPorts, numWritePorts, numPhyPregs, pregWB) {

  val io_mx_Wb = IO(new Bundle() {
    val mxWriteBackInfo = new Bundle {
      val mxFromIntIsZero  = Input(Bool())
      val mxFromIntIsMxmax = Input(Bool())
      val mxFromMfIsZero   = Input(Bool())
      val mxFromMfIsMxmax  = Input(Bool())
    }
  })
  val io_mx_read = IO(new Bundle() {
    val mxReadInfo = Vec(numReadPorts, new MxBusyTableReadIO)
  })

  var intSchdMxWbPort = p(XSCoreParamsKey).intSchdMxWbPort
  var mfSchdMxWbPort = p(XSCoreParamsKey).mfSchdMxWbPort

  val zeroTableUpdate = Wire(Vec(numPhyPregs, Bool()))
  val mxmaxTableUpdate = Wire(Vec(numPhyPregs, Bool()))

  val intMxWb = Mux(io.wbPregs(intSchdMxWbPort).valid, UIntToOH(io.wbPregs(intSchdMxWbPort).bits), 0.U)
  val mfMxWb = Mux(io.wbPregs(mfSchdMxWbPort).valid, UIntToOH(io.wbPregs(mfSchdMxWbPort).bits), 0.U)

  val zeroTable = VecInit((0 until numPhyPregs).zip(zeroTableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || intMxWb(idx) || mfMxWb(idx))
  }).asUInt
  val mxmaxTable = VecInit((0 until numPhyPregs).zip(mxmaxTableUpdate).map{ case (idx, update) =>
    RegEnable(update, 0.U(1.W), allocMask(idx) || ldCancelMask(idx) || intMxWb(idx) || mfMxWb(idx))
  }).asUInt


  zeroTableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(intMxWb(idx)) {
      // int schd mtilex write back, check whether the mtilex is zero
      update := !io_mx_Wb.mxWriteBackInfo.mxFromIntIsZero
    }.elsewhen(mfMxWb(idx)) {
      // mf schd mtilex write back, check whether the mtilex is zero
      update := !io_mx_Wb.mxWriteBackInfo.mxFromMfIsZero
    }.elsewhen(allocMask(idx) || ldCancelMask(idx)) {
      update := true.B
    }.otherwise {
      update := zeroTable(idx)
    }
  }

  mxmaxTableUpdate.zipWithIndex.foreach{ case (update, idx) =>
    when(intMxWb(idx)) {
      // int schd vl write back, check whether the vl is vlmax
      update := !io_mx_Wb.mxWriteBackInfo.mxFromIntIsMxmax
    }.elsewhen(mfMxWb(idx)) {
      // vf schd vl write back, check whether the vl is vlmax
      update := !io_mx_Wb.mxWriteBackInfo.mxFromMfIsMxmax
    }.elsewhen(allocMask(idx) || ldCancelMask(idx)) {
      update := true.B
    }.otherwise {
      update := mxmaxTable(idx)
    }
  }

  io_mx_read.mxReadInfo.zip(io.read).foreach{ case (mxRes, res) =>
    mxRes.is_zero := !zeroTable(res.req)
    mxRes.is_mxmax := !mxmaxTable(res.req)
  }
}