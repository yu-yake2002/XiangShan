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
  val newCommit: UInt = Input(UInt(3.W))
}

class UnifiedPredictor(implicit p: Parameters) extends Predictor {
  assert(EnableUnifiedFtb)
  override lazy val io = IO(new UnifiedPredictorIO)
  override lazy val predictors: BasePredictor = Module(new UnifiedComposer)

  predictors match {
    case unifiedComp: UnifiedComposer =>
      io.readReq <> unifiedComp.io.readReq
      io.readResp <> unifiedComp.io.readResp
      io.writeReq <> unifiedComp.io.writeReq
      unifiedComp.io.newCommit := io.newCommit
    case _ =>
  }
}

class UnifiedComposerIO(implicit p: Parameters) extends BasePredictorIO {
  val readReq: DecoupledIO[UnifiedFtbReadReq] = DecoupledIO(new UnifiedFtbReadReq)
  val readResp: ValidIO[UnifiedFtbReadResp] = Flipped(ValidIO(new UnifiedFtbReadResp))
  val writeReq: DecoupledIO[UnifiedFtbWriteReq] = DecoupledIO(new UnifiedFtbWriteReq)
  val newCommit: UInt = Input(UInt(3.W))
}

class UnifiedComposer(implicit p: Parameters) extends Composer{
  assert(EnableUnifiedFtb)
  override lazy val io = IO(new UnifiedComposerIO)
  val ftbCtrl = Module(new UnifiedController()(p))
  for (c <- components) {
    c match {
      case ftb: UnifiedFtb =>
        io.readReq <> ftb.io.readReq
        io.readResp <> ftb.io.readResp
        io.writeReq <> ftb.io.writeReq
        ftb.io.prefetchCtrler := ftbCtrl.io.gates(0)
        ftb.io.generateCtrler := ftbCtrl.io.gates(1)
      case _ =>
    }
  }
  ftbCtrl.io.newCommits := io.newCommit
}
