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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.frontend._

class UnifiedPredictorIO(implicit p: Parameters) extends PredictorIO {
  val readReq: DecoupledIO[UnifiedFtbReadReq] = DecoupledIO(new UnifiedFtbReadReq)
  val readResp: ValidIO[UnifiedFtbReadResp] = Flipped(ValidIO(new UnifiedFtbReadResp))
  val writeReq: DecoupledIO[UnifiedFtbWriteReq] = DecoupledIO(new UnifiedFtbWriteReq)
}

class UnifiedPredictor(implicit p: Parameters) extends Predictor{
  assert(EnableUnifiedFtb)
  override lazy val io = IO(new UnifiedPredictorIO)
  dontTouch(io)
  override lazy val predictors: BasePredictor = Module(
    if (EnableUnifiedFtb) {
      new UnifiedComposer
    } else {
      if (useBPD) new Composer else new FakePredictor
    }
  )
  predictors match {
    case unifiedComp: UnifiedComposer =>
      io.readReq <> unifiedComp.io.readReq
      io.readResp <> unifiedComp.io.readResp
      io.writeReq <> unifiedComp.io.writeReq
    case _ =>
  }
}

class UnifiedComposerIO(implicit p: Parameters) extends BasePredictorIO {
  val readReq: DecoupledIO[UnifiedFtbReadReq] = DecoupledIO(new UnifiedFtbReadReq)
  val readResp: ValidIO[UnifiedFtbReadResp] = Flipped(ValidIO(new UnifiedFtbReadResp))
  val writeReq: DecoupledIO[UnifiedFtbWriteReq] = DecoupledIO(new UnifiedFtbWriteReq)
}

class UnifiedComposer(implicit p: Parameters) extends Composer{
  assert(EnableUnifiedFtb)
  override lazy val io = IO(new UnifiedComposerIO)
  dontTouch(io)
  for (c <- components) {
    c match {
      case ftb: UnifiedFtb =>
        io.readReq <> ftb.io.readReq
        io.readResp <> ftb.io.readResp
        io.writeReq <> ftb.io.writeReq
      case _ =>
    }
  }
}
