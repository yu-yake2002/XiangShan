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

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{WakeUpConfig, WbArbiterParams}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.issue._
import xiangshan.backend.regfile._
import xiangshan.{DebugOptionsKey, XSCoreParamsKey}

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

case class BackendParams(
  schdParams : Map[SchedulerType, SchdBlockParams],
  pregParams : Seq[PregParams],
  iqWakeUpParams : Seq[WakeUpConfig],
) {

  def debugEn(implicit p: Parameters): Boolean = p(DebugOptionsKey).EnableDifftest

  def basicDebugEn(implicit p: Parameters): Boolean = p(DebugOptionsKey).AlwaysBasicDiff || debugEn

  val copyPdestInfo = mutable.HashMap[Int, (Int, Int)]()

  def updateCopyPdestInfo: Unit = allExuParams.filter(_.copyWakeupOut).map(x => getExuIdx(x.name) -> (x.copyDistance, -1)).foreach { x =>
    copyPdestInfo.addOne(x)
  }
  def isCopyPdest(exuIdx: Int): Boolean = {
    copyPdestInfo.contains(exuIdx)
  }
  def connectWakeup(exuIdx: Int): Unit = {
    println(s"[Backend] copyPdestInfo ${copyPdestInfo}")
    if (copyPdestInfo.contains(exuIdx)) {
      println(s"[Backend] exuIdx ${exuIdx} be connected, old info ${copyPdestInfo(exuIdx)}")
      val newInfo = exuIdx -> (copyPdestInfo(exuIdx)._1, copyPdestInfo(exuIdx)._2 + 1)
      copyPdestInfo.remove(exuIdx)
      copyPdestInfo += newInfo
      println(s"[Backend] exuIdx ${exuIdx} be connected, new info ${copyPdestInfo(exuIdx)}")
    }
  }
  def getCopyPdestIndex(exuIdx: Int): Int = {
    copyPdestInfo(exuIdx)._2 / copyPdestInfo(exuIdx)._1
  }
  def intSchdParams = schdParams.get(IntScheduler())
  def fpSchdParams = schdParams.get(FpScheduler())
  def vfSchdParams = schdParams.get(VfScheduler())
  def mfSchdParams = schdParams.get(MfScheduler())
  def memSchdParams = schdParams.get(MemScheduler())
  def allSchdParams: Seq[SchdBlockParams] =
    (Seq(intSchdParams) :+ fpSchdParams :+ vfSchdParams :+ mfSchdParams :+ memSchdParams)
    .filter(_.nonEmpty)
    .map(_.get)
  def allIssueParams: Seq[IssueBlockParams] =
    allSchdParams.map(_.issueBlockParams).flatten
  def allExuParams: Seq[ExeUnitParams] =
    allIssueParams.map(_.exuBlockParams).flatten

  // filter not fake exu unit
  def allRealExuParams =
    allExuParams.filterNot(_.fakeUnit)

  def intPregParams: IntPregParams = pregParams.collectFirst { case x: IntPregParams => x }.get
  def fpPregParams: FpPregParams = pregParams.collectFirst { case x: FpPregParams => x }.get
  def vfPregParams: VfPregParams = pregParams.collectFirst { case x: VfPregParams => x }.get
  def v0PregParams: V0PregParams = pregParams.collectFirst { case x: V0PregParams => x }.get
  def vlPregParams: VlPregParams = pregParams.collectFirst { case x: VlPregParams => x }.get
  def mxPregParams: MxPregParams = pregParams.collectFirst { case x: MxPregParams => x }.get
  def getPregParams: Map[DataConfig, PregParams] = {
    pregParams.map(x => (x.dataCfg, x)).toMap
  }

  def pregIdxWidth = pregParams.map(_.addrWidth).max

  def numSrc      : Int = allSchdParams.map(_.issueBlockParams.map(_.numSrc).max).max
  def numRegSrc   : Int = allSchdParams.map(_.issueBlockParams.map(_.numRegSrc).max).max
  def numIntRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numIntSrc).max).max
  def numFpRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numFpSrc).max).max
  def numVecRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numVecSrc).max).max


  def AluCnt = allSchdParams.map(_.AluCnt).sum
  def StaCnt = allSchdParams.map(_.StaCnt).sum
  def StdCnt = allSchdParams.map(_.StdCnt).sum
  def LduCnt = allSchdParams.map(_.LduCnt).sum
  def HyuCnt = allSchdParams.map(_.HyuCnt).sum
  def VlduCnt = allSchdParams.map(_.VlduCnt).sum
  def VstuCnt = allSchdParams.map(_.VstuCnt).sum
  def MlsCnt = allSchdParams.map(_.MlsCnt).sum
  def LsExuCnt = StaCnt + LduCnt + HyuCnt
  def LdExuCnt = LduCnt + HyuCnt + MlsCnt
  def LdWakeupCnt = LduCnt + HyuCnt
  def LdPfTrainCnt = LduCnt + HyuCnt
  def StaExuCnt = StaCnt + HyuCnt
  def JmpCnt = allSchdParams.map(_.JmpCnt).sum
  def BrhCnt = allSchdParams.map(_.BrhCnt).sum
  def CsrCnt = allSchdParams.map(_.CsrCnt).sum
  def IqCnt = allSchdParams.map(_.issueBlockParams.length).sum

  def numPcMemReadPort = allExuParams.filter(_.needPc).size
  def numTargetReadPort = allRealExuParams.count(x => x.needTarget)

  def numPregRd(dataCfg: DataConfig) = this.getRfReadSize(dataCfg)
  def numPregWb(dataCfg: DataConfig) = this.getRfWriteSize(dataCfg)

  def numNoDataWB = allSchdParams.map(_.numNoDataWB).sum
  def numExu = allSchdParams.map(_.numExu).sum

  def numException = allRealExuParams.count(_.exceptionOut.nonEmpty)

  def numRedirect = 1 // only for ahead info to frontend

  def numLoadDp = memSchdParams.get.issueBlockParams.filter(x => x.isLdAddrIQ || x.isHyAddrIQ).map(_.numEnq).sum

  def numStoreDp = memSchdParams.get.issueBlockParams.filter(x => x.isStAddrIQ || x.isHyAddrIQ).map(_.numEnq).sum

  def numMlsDp = memSchdParams.get.issueBlockParams.filter(x => x.isMatrixMemIQ).map(_.numEnq).sum

  def genIntIQValidNumBundle(implicit p: Parameters) = {
    this.intSchdParams.get.issueBlockParams.map(x => Vec(x.numDeq, UInt((x.numEntries).U.getWidth.W)))
  }

  def genFpIQValidNumBundle(implicit p: Parameters) = {
    this.fpSchdParams.get.issueBlockParams.map(x => Vec(x.numDeq, UInt((x.numEntries).U.getWidth.W)))
  }

  def genMfIQValidNumBundle(implicit p: Parameters) = {
    this.mfSchdParams.get.issueBlockParams.map(x => Vec(x.numDeq, UInt((x.numEntries).U.getWidth.W)))
  }

  def genIntWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getIntRfWriteSize)(new RfWritePortWithConfig(IntData(), intPregParams.addrWidth))
  }

  def genFpWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getFpRfWriteSize)(new RfWritePortWithConfig(FpData(), fpPregParams.addrWidth))
  }

  def genVfWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getVfRfWriteSize)(new RfWritePortWithConfig(VecData(), vfPregParams.addrWidth))
  }

  def genV0WriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getV0RfWriteSize)(new RfWritePortWithConfig(V0Data(), v0PregParams.addrWidth))
  }

  def genVlWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getVlRfWriteSize)(new RfWritePortWithConfig(VlData(), vlPregParams.addrWidth))
  }

  def genMxWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getMxRfWriteSize)(new RfWritePortWithConfig(MxData(), mxPregParams.addrWidth))
  }

  def genWriteBackBundles(implicit p: Parameters): Seq[RfWritePortWithConfig] = {
    genIntWriteBackBundle ++ genVfWriteBackBundle
  }

  def genWrite2CtrlBundles(implicit p: Parameters): MixedVec[ValidIO[ExuOutput]] = {
    MixedVec(allSchdParams.map(_.genExuOutputValidBundle.flatten).flatten)
  }

  def getIntWbArbiterParams: WbArbiterParams = {
    val intWbCfgs: Seq[IntWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(_.writeInt)).map(_.asInstanceOf[IntWB])
    datapath.WbArbiterParams(intWbCfgs, intPregParams, this)
  }

  def getVfWbArbiterParams: WbArbiterParams = {
    val vfWbCfgs: Seq[VfWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVec)).map(_.asInstanceOf[VfWB])
    datapath.WbArbiterParams(vfWbCfgs, vfPregParams, this)
  }

  def getFpWbArbiterParams: WbArbiterParams = {
    val fpWbCfgs: Seq[FpWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeFp)).map(_.asInstanceOf[FpWB])
    datapath.WbArbiterParams(fpWbCfgs, fpPregParams, this)
  }

  def getV0WbArbiterParams: WbArbiterParams = {
    val v0WbCfgs: Seq[V0WB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeV0)).map(_.asInstanceOf[V0WB])
    datapath.WbArbiterParams(v0WbCfgs, v0PregParams, this)
  }

  def getMxWbArbiterParams: WbArbiterParams = {
    val mxWbCfgs: Seq[MxWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeMx)).map(_.asInstanceOf[MxWB])
    datapath.WbArbiterParams(mxWbCfgs, mxPregParams, this)
  }

  def getVlWbArbiterParams: WbArbiterParams = {
    val vlWbCfgs: Seq[VlWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVl)).map(_.asInstanceOf[VlWB])
    datapath.WbArbiterParams(vlWbCfgs, vlPregParams, this)
  }

  /**
    * Get regfile read port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getRdPortParams(dataCfg: DataConfig) = {
    // port -> Seq[exuIdx, priority]
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allRealExuParams
      .flatMap(x => x.rfrPortConfigs.flatten.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.getDataConfig == dataCfg }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2).sortBy({ case (priority, _) => priority })))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  /**
    * Get regfile write back port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getWbPortParams(dataCfg: DataConfig) = {
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allRealExuParams
      .flatMap(x => x.wbPortConfigs.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.dataCfg == dataCfg }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2)))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  def getRdPortIndices(dataCfg: DataConfig) = {
    this.getRdPortParams(dataCfg).map(_._1)
  }

  def getWbPortIndices(dataCfg: DataConfig) = {
    this.getWbPortParams(dataCfg).map(_._1)
  }

  def getRdCfgs[T <: RdConfig](implicit tag: ClassTag[T]): Seq[Seq[Seq[RdConfig]]] = {
    val rdCfgs: Seq[Seq[Seq[RdConfig]]] = allIssueParams.map(
      _.exuBlockParams.map(
        _.rfrPortConfigs.map(
          _.collectFirst{ case x: T => x }
            .getOrElse(NoRD())
        )
      )
    )
    rdCfgs
  }

  def getAllWbCfgs: Seq[Seq[Set[PregWB]]] = {
    allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.toSet))
  }

  def getWbCfgs[T <: PregWB](implicit tag: ClassTag[T]): Seq[Seq[PregWB]] = {
    val wbCfgs: Seq[Seq[PregWB]] = allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.collectFirst{ case x: T => x }.getOrElse(NoWB())))
    wbCfgs
  }

  /**
    * Get size of read ports of int regfile
    *
    * @return if [[IntPregParams.numRead]] is [[None]], get size of ports in [[IntRD]]
    */
  def getIntRfReadSize = {
    this.intPregParams.numRead.getOrElse(this.getRdPortIndices(IntData()).size)
  }

  /**
    * Get size of write ports of int regfile
    *
    * @return if [[IntPregParams.numWrite]] is [[None]], get size of ports in [[IntWB]]
    */
  def getIntRfWriteSize = {
    this.intPregParams.numWrite.getOrElse(this.getWbPortIndices(IntData()).size)
  }

  /**
   * Get size of write ports of fp regfile
   *
   * @return if [[FpPregParams.numWrite]] is [[None]], get size of ports in [[FpWB]]
   */
  def getFpRfWriteSize = {
    this.fpPregParams.numWrite.getOrElse(this.getWbPortIndices(FpData()).size)
  }

  /**
    * Get size of read ports of vec regfile
    *
    * @return if [[VfPregParams.numRead]] is [[None]], get size of ports in [[VfRD]]
    */
  def getVfRfReadSize = {
    this.vfPregParams.numRead.getOrElse(this.getRdPortIndices(VecData()).size)
  }

  /**
    * Get size of write ports of vec regfile
    *
    * @return if [[VfPregParams.numWrite]] is [[None]], get size of ports in [[VfWB]]
    */
  def getVfRfWriteSize = {
    this.vfPregParams.numWrite.getOrElse(this.getWbPortIndices(VecData()).size)
  }

  def getV0RfWriteSize = {
    this.v0PregParams.numWrite.getOrElse(this.getWbPortIndices(V0Data()).size)
  }

  def getVlRfWriteSize = {
    this.vlPregParams.numWrite.getOrElse(this.getWbPortIndices(VlData()).size)
  }

  def getMxRfWriteSize = {
    this.mxPregParams.numWrite.getOrElse(this.getWbPortIndices(MxData()).size)
  }

  def getRfReadSize(dataCfg: DataConfig) = {
    dataCfg match{
      case IntData() => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case FpData()  => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case VecData() => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case V0Data() => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case VlData() => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case MxData() => this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
      case _ => throw new IllegalArgumentException(s"DataConfig ${dataCfg} can not get RfReadSize")
    }
  }

  def getRfWriteSize(dataCfg: DataConfig) = {
    this.getPregParams(dataCfg).numWrite.getOrElse(this.getWbPortIndices(dataCfg).size)
  }


  /**
    * Get size of read ports of int regcache
    */
  def getIntExuRCReadSize = {
    this.allExuParams.filter(x => x.isIntExeUnit).map(_.numIntSrc).reduce(_ + _)
  }

  def getMemExuRCReadSize = {
    this.allExuParams.filter(x => x.isMemExeUnit && x.readIntRf && !x.readMxRf).map(_.numIntSrc).reduce(_ + _)
  }

  /**
    * Get size of write ports of int regcache
    */
  def getIntExuRCWriteSize = {
    this.allExuParams.filter(x => x.isIntExeUnit && x.isIQWakeUpSource).size
  }

  def getMemExuRCWriteSize = {
    this.allExuParams.filter(x => x.isMemExeUnit && x.isIQWakeUpSource && x.readIntRf).size
  }

  def getExuIdx(name: String): Int = {
    val exuParams = allRealExuParams
    if (name != "WB") {
      val foundExu = exuParams.find(_.name == name)
      require(foundExu.nonEmpty, s"exu $name not find")
      foundExu.get.exuIdx
    } else
      -1
  }

  def getExuName(idx: Int): String = {
    val exuParams = allRealExuParams
    exuParams(idx).name
  }

  def getExuParamByName(name: String): ExeUnitParams = {
    val exuParams = allExuParams
    exuParams.find(_.name == name).get
  }

  def getLdExuIdx(exu: ExeUnitParams): Int = {
    val ldExuParams = allRealExuParams.filter(x => x.hasHyldaFu || x.hasLoadFu)
    ldExuParams.indexOf(exu)
  }

  def getIntWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getIntWBPort.getOrElse(IntWB(port = -1)).port).filter(_._1 != -1)
  def getFpWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getFpWBPort.getOrElse(FpWB(port = -1)).port).filter(_._1 != -1)
  def getVfWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getVfWBPort.getOrElse(VfWB(port = -1)).port).filter(_._1 != -1)
  def getV0WBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getV0WBPort.getOrElse(V0WB(port = -1)).port).filter(_._1 != -1)
  def getVlWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getVlWBPort.getOrElse(VlWB(port = -1)).port).filter(_._1 != -1)
  def getMxWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allRealExuParams.groupBy(x => x.getMxWBPort.getOrElse(MxWB(port = -1)).port).filter(_._1 != -1)

  private def isContinuous(portIndices: Seq[Int]): Boolean = {
    val portIndicesSet = portIndices.toSet
    portIndicesSet.min == 0 && portIndicesSet.max == portIndicesSet.size - 1
  }

  def configChecks = {
    checkReadPortContinuous
    checkWritePortContinuous
    configCheck
  }

  def checkReadPortContinuous = {
    pregParams.filterNot(_.isFake).foreach { x =>
      if (x.numRead.isEmpty) {
        val portIndices: Seq[Int] = getRdPortIndices(x.dataCfg)
        require(isContinuous(portIndices),
          s"The read ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numRead of ${x.getClass.getSimpleName} is None. The read port indices are $portIndices")
      }
    }
  }

  def checkWritePortContinuous = {
    pregParams.filterNot(_.isFake).foreach { x =>
      if (x.numWrite.isEmpty) {
        val portIndices: Seq[Int] = getWbPortIndices(x.dataCfg)
        require(
          isContinuous(portIndices),
          s"The write ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numWrite of ${x.getClass.getSimpleName} is None. The write port indices are $portIndices"
        )
      }
    }
  }

  def configCheck = {
    // check 0
    val maxPortSource = 4

    allRealExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: IntWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Int WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }
    allRealExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: VfWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Vf  WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }

    // check 1
    // if some exus share the same wb port and rd ports,
    // the exu with high priority at wb must also have high priority at rd.
    val wbTypes = Seq(IntWB(), FpWB(), VfWB(), MxWB())
    val rdTypes = Seq(IntRD(), FpRD(), VfRD(), MxRD())
    for(wbType <- wbTypes){
      for(rdType <- rdTypes){
        println(s"[BackendParams] wbType: ${wbType}, rdType: ${rdType}")
        allRealExuParams.map {
          case exuParam =>
            val wbPortConfigs = exuParam.wbPortConfigs
            val wbConfigs = wbType match{
              case _: IntWB => wbPortConfigs.collectFirst { case x: IntWB => x }
              case _: FpWB  => wbPortConfigs.collectFirst { case x: FpWB => x }
              case _: VfWB  => wbPortConfigs.collectFirst { case x: VfWB => x }
              case _: MxWB  => wbPortConfigs.collectFirst { case x: MxWB => x }
              case _        => None
            }
            val rfReadPortConfigs = exuParam.rfrPortConfigs
            val rdConfigs = rdType match{
              case _: IntRD => rfReadPortConfigs.flatten.filter(_.isInstanceOf[IntRD])
              case _: FpRD  => rfReadPortConfigs.flatten.filter(_.isInstanceOf[FpRD])
              case _: VfRD  => rfReadPortConfigs.flatten.filter(_.isInstanceOf[VfRD])
              case _: MxRD  => rfReadPortConfigs.flatten.filter(_.isInstanceOf[MxRD])
              case _        => Seq()
            }
            (wbConfigs, rdConfigs)
        }.filter(_._1.isDefined)
          .sortBy(_._1.get.priority)
          .groupBy(_._1.get.port).map { case (wbPort, intWbRdPairs) =>
            val rdCfgs = intWbRdPairs.map(_._2).flatten
            println(s"[BackendParams] wb port ${wbPort} rdcfgs: ${rdCfgs}")
            rdCfgs.groupBy(_.port).foreach { case (p, rdCfg) =>
              //println(s"[BackendParams] rdport: ${p}, cfgs: ${rdCfg}")
              rdCfg.zip(rdCfg.drop(1)).foreach { case (cfg0, cfg1) => assert(cfg0.priority <= cfg1.priority, s"an exu has high priority at ${wbType} wb port ${wbPort}, but has low priority at ${rdType} rd port ${p}") }
            }
        }
      }
    }
  }
}
