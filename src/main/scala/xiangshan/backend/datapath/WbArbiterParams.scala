package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3.Output
import chisel3.util.{DecoupledIO, MixedVec, ValidIO, log2Up}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.WriteBackBundle
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.regfile.PregParams

case class WbArbiterParams(
  wbCfgs    : Seq[PregWB],
  pregParams: PregParams,
  backendParams: BackendParams,
) {

  def numIn = wbCfgs.length

  def numOut = wbCfgs.head match {
    case _: WbConfig.IntWB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(IntData()).size)
    case _: WbConfig.FpWB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(FpData()).size)
    case _: WbConfig.VfWB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(VecData()).size)
    case _: WbConfig.V0WB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(V0Data()).size)
    case _: WbConfig.VlWB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(VlData()).size)
    case _: WbConfig.MxWB => pregParams.numWrite.getOrElse(backendParams.getWbPortIndices(MxData()).size)
    case x =>
      assert(assertion = false, s"the WbConfig in WbArbiterParams should be either IntWB or FpWB or VfWB or V0WB or VlWB or MxWB, found ${x.getClass}")
      0
  }

  def dataWidth = pregParams.dataCfg.dataWidth

  def addrWidth = log2Up(pregParams.numEntries)

  def genInput(implicit p: Parameters) = {
    MixedVec(wbCfgs.map(x => DecoupledIO(new WriteBackBundle(x, backendParams))))
  }

  def genOutput(implicit p: Parameters): MixedVec[ValidIO[WriteBackBundle]] = {
    Output(MixedVec(Seq.tabulate(numOut) {
      x =>
        ValidIO(new WriteBackBundle(
          wbCfgs.head.dataCfg match {
            case IntData() => IntWB(port = x)
            case FpData()  => FpWB(port = x)
            case VecData() => VfWB(port = x)
            case V0Data()  => V0WB(port = x)
            case VlData()  => VlWB(port = x)
            case MxData()  => MxWB(port = x)
            case _ => ???
          },
          backendParams
        )
        )
    }
    )
    )
  }
}
