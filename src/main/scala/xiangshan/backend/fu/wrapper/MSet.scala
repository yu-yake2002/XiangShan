package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{MatrixSETOpType, CSROpType}
import xiangshan.backend.decode.{Imm_MSET, Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.matrix.Bundles.MsetMType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit, MsetMtilexModule, MsetMtypeModule, MtypeStruct}
import xiangshan.backend.fu.matrix.Bundles.MConfig
import xiangshan.backend.fu.matrix.Bundles.MType

class MSetMtilexBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetModule = Module(new MsetMtilexModule)

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // Get atx from msettile{m|k|n}[i]
  protected val atxImm = Imm_MSET().getAtx(in.data.src(1))
  protected val atx = Mux(MatrixSETOpType.isMsettilexi(in.ctrl.fuOpType), atxImm, in.data.src(0))
  
  // Get old mtype
  val oldMtype = in.data.src(3).asTypeOf(MType())

  msetModule.io.in.func := in.ctrl.fuOpType
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

/**
  * Wrapper of MsetMtilexModule
  * This fu is uop of mset which reads two int regs and writes one int regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.umsetrd_i]], <br/>
  * [[MatrixSETOpType.umsetrd_x]], <br/>
  * [[MatrixSETOpType.umsetrd_vlmax_i]], <br/>
  * [[MatrixSETOpType.umsetrd_vlmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetMtilexRiWi(cfg: FuConfig)(implicit p: Parameters) extends MSetMtilexBase(cfg) {
  msetModule.io.in.atx := atx
  msetModule.io.in.mtype := oldMtype

  out.res.data := msetModule.io.out.mtilex
}

/**
  * Wrapper of MsetMtilexModule
  * This fu is uop of vset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.umsetmtilem_i]], <br/>
  * [[MatrixSETOpType.umsetmtilem_x]], <br/>
  * [[MatrixSETOpType.umsetmtilen_i]], <br/>
  * [[MatrixSETOpType.umsetmtilen_x]], <br/>
  * [[MatrixSETOpType.umsetmtilek_i]], <br/>
  * [[MatrixSETOpType.umsetmtilek_x]], <br/>
  * [[MatrixSETOpType.umsetmtilem_mtilemmax_i]], <br/>
  * [[MatrixSETOpType.umsetmtilem_mtilemmax_x]], <br/>
  * [[MatrixSETOpType.umsetmtilen_mtilenmax_i]], <br/>
  * [[MatrixSETOpType.umsetmtilen_mtilenmax_x]], <br/>
  * [[MatrixSETOpType.umsetmtilek_mtilekmax_i]], <br/>
  * [[MatrixSETOpType.umsetmtilek_mtilekmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetMtilexRiWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtilexBase(cfg) {
  msetModule.io.in.atx := atx
  msetModule.io.in.mtype := oldMtype
  val mtilex = msetModule.io.out.mtilex
  val tilexmax = msetModule.io.out.txmax
  val isMsettilex = MatrixSETOpType.isMsettilex(in.ctrl.fuOpType)

  out.res.data := mtilex

  if (cfg.writeMtilexRf) io.mtilex.get.bits := msetModule.io.out.mtilex
  if (cfg.writeMtilexRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMtilexRf) io.mtilexIsZero.get := io.out.valid && mtilex === 0.U
  if (cfg.writeMtilexRf) io.mtilexIsMtilexmax.get := io.out.valid && mtilex === tilexmax
}

/**
  * Wrapper of MsetMtilexModule
  * This fu is uop of mset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.uvsetvcfg_vv]], <br/>
  * [[MatrixSETOpType.uvsetvcfg_keep_v]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetMtilexRmfWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtilexBase(cfg) {
  val oldMtilex = in.data.src(4)
  msetModule.io.in.atx := oldMtilex
  msetModule.io.in.mtype := oldMtype

  val mtilex = msetModule.io.out.mtilex
  val txmax = msetModule.io.out.txmax
  val isMsettilex = MatrixSETOpType.isMsettilex(in.ctrl.fuOpType)
  val isMreadMtilex = MatrixSETOpType.isMreadMtilex(in.ctrl.fuOpType)

  // Select output 
  out.res.data := Mux(isMreadMtilex, oldMtilex, mtilex)

  if (cfg.writeMtilexRf) io.mtilex.get.bits := msetModule.io.out.mtilex
  if (cfg.writeMtilexRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMtilexRf) io.mtilexIsZero.get := io.out.valid && !isMreadMtilex && mtilex === 0.U
  if (cfg.writeMtilexRf) io.mtilexIsMtilexmax.get := io.out.valid && !isMreadMtilex && mtilex === txmax
}

class MSetMtypeBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetMtypeModule = Module(new MsetMtypeModule)

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  protected val newMtypeImm = Imm_MSET().getAtx(in.data.src(1))
  protected val newMtype = Mux(MatrixSETOpType.isMsetMtypeFromImm(in.ctrl.fuOpType), newMtypeImm, in.data.src(0))
  
  msetMtypeModule.io.in.newmtype := newMtype
  msetMtypeModule.io.in.oldmtype := in.data.src(3).asTypeOf(MType())
  msetMtypeModule.io.in.mask     := in.data.src(4)
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

class MSetMtypeRiWi(cfg: FuConfig)(implicit p: Parameters) extends MSetMtypeBase(cfg) {
  // TODO: Implement MsetMtypeRiWi
}

class MSetMtypeRiWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtypeBase(cfg) {
  // TODO: Implement MsetMtypeRiWmf
}