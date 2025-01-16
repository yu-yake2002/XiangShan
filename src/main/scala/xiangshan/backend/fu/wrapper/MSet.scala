package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{MatrixSETOpType, CSROpType}
import xiangshan.backend.decode.{Imm_MSET, Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.matrix.Bundles.MsetMType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit, MsetModule, MtypeStruct}
import xiangshan.backend.fu.matrix.Bundles.MConfig
import xiangshan.backend.fu.matrix.Bundles.MType

class MSetMtileBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetModule = Module(new MsetModule)

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
  * Wrapper of MsetModule
  * This fu is uop of mset which reads two int regs and writes one int regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.umsetrd_xi]], <br/>
  * [[MatrixSETOpType.umsetrd_xx]], <br/>
  * [[MatrixSETOpType.umsetrd_vlmax_i]], <br/>
  * [[MatrixSETOpType.umsetrd_vlmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetRiWi(cfg: FuConfig)(implicit p: Parameters) extends MSetMtileBase(cfg) {
  msetModule.io.in.atx := atx
  msetModule.io.in.mtype := oldMtype

  out.res.data := msetModule.io.out.outval
}

/**
  * Wrapper of VsetModule
  * This fu is uop of vset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.umsetmtilem_xi]], <br/>
  * [[MatrixSETOpType.umsetmtilem_xx]], <br/>
  * [[MatrixSETOpType.umsetmtilen_xi]], <br/>
  * [[MatrixSETOpType.umsetmtilen_xx]], <br/>
  * [[MatrixSETOpType.umsetmtilek_xi]], <br/>
  * [[MatrixSETOpType.umsetmtilek_xx]], <br/>
  * [[MatrixSETOpType.umsettilem_mtilemmax_i]], <br/>
  * [[MatrixSETOpType.umsettilem_mtilemmax_x]], <br/>
  * [[MatrixSETOpType.umsettilen_mtilenmax_i]], <br/>
  * [[MatrixSETOpType.umsettilen_mtilenmax_x]], <br/>
  * [[MatrixSETOpType.umsettilek_mtilekmax_i]], <br/>
  * [[MatrixSETOpType.umsettilek_mtilekmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetRiWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtileBase(cfg) {
  msetModule.io.in.atx := atx
  msetModule.io.in.mtype := oldMtype
  val mtilex = msetModule.io.out.outval
  val tilexmax = msetModule.io.out.txmax
  val isMsettilex = MatrixSETOpType.isMsettilex(in.ctrl.fuOpType)

  out.res.data := mtilex

  if (cfg.writeMtilexRf) io.mtilex.get.bits := msetModule.io.out.outval
  if (cfg.writeMtilexRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMtilexRf) io.mtilexIsZero.get := io.out.valid && mtilex === 0.U
  if (cfg.writeMtilexRf) io.mtilexIsMtilexmax.get := io.out.valid && mtilex === tilexmax
}

/**
  * Wrapper of MsetModule
  * This fu is uop of mset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[MatrixSETOpType.uvsetvcfg_vv]], <br/>
  * [[MatrixSETOpType.uvsetvcfg_keep_v]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetRvfWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtileBase(cfg) {
  val oldMtilex = in.data.src(4)
  msetModule.io.in.atx := oldMtilex
  msetModule.io.in.mtype := oldMtype

  val mtilex = msetModule.io.out.outval
  val txmax = msetModule.io.out.txmax
  val isMsettilex = MatrixSETOpType.isMsettilex(in.ctrl.fuOpType)
  val isMreadMtilex = MatrixSETOpType.isMreadMtilex(in.ctrl.fuOpType)

  // Select output 
  out.res.data := Mux(isMreadMtilex, oldMtilex, mtilex)

  if (cfg.writeMtilexRf) io.mtilex.get.bits := msetModule.io.out.outval
  if (cfg.writeMtilexRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMtilexRf) io.mtilexIsZero.get := io.out.valid && !isMreadMtilex && mtilex === 0.U
  if (cfg.writeMtilexRf) io.mtilexIsMtilexmax.get := io.out.valid && !isMreadMtilex && mtilex === txmax
}