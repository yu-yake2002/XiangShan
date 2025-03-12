package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{MSETtilexOpType, MSETtypeOpType, CSROpType}
import xiangshan.backend.decode.{Imm_MSET, Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.matrix.Bundles.MsetMType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.{MsetMtilexModule, MsetMtypeBaseModule, MsetMtypeDummyModule, MsetMtypeModule, MtypeStruct}
import xiangshan.backend.fu.matrix.Bundles.MConfig
import xiangshan.backend.fu.matrix.Bundles.MType
import chisel3.util.switch

class MSetMtilexBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetModule = Module(new MsetMtilexModule)

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // Get atx from msettile{m|k|n}[i]
  protected val atxImm = Imm_MSET().getAtx(in.data.src(0))
  protected val atx = Mux(MSETtilexOpType.isMsettilexi(in.ctrl.fuOpType), atxImm, in.data.src(0))
  
  // Get old mtype
  val oldMtype = MType.toMsetMType(in.ctrl.mpu.get.mtype)

  msetModule.io.in.func := in.ctrl.fuOpType
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

/**
  * Wrapper of MsetMtilexModule
  * This fu is uop of mset which reads two int regs and writes one int regs.<br>
  * uop: <br/>
  * [[MSETtilexOpType.umsetrd_i]], <br/>
  * [[MSETtilexOpType.umsetrd_x]], <br/>
  * [[MSETtilexOpType.umsetrd_vlmax_i]], <br/>
  * [[MSETtilexOpType.umsetrd_vlmax_x]], <br/>
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
  * [[MSETtilexOpType.umsetmtilem_i]], <br/>
  * [[MSETtilexOpType.umsetmtilem_x]], <br/>
  * [[MSETtilexOpType.umsetmtilen_i]], <br/>
  * [[MSETtilexOpType.umsetmtilen_x]], <br/>
  * [[MSETtilexOpType.umsetmtilek_i]], <br/>
  * [[MSETtilexOpType.umsetmtilek_x]], <br/>
  * [[MSETtilexOpType.umsetmtilem_mtilemmax_i]], <br/>
  * [[MSETtilexOpType.umsetmtilem_mtilemmax_x]], <br/>
  * [[MSETtilexOpType.umsetmtilen_mtilenmax_i]], <br/>
  * [[MSETtilexOpType.umsetmtilen_mtilenmax_x]], <br/>
  * [[MSETtilexOpType.umsetmtilek_mtilekmax_i]], <br/>
  * [[MSETtilexOpType.umsetmtilek_mtilekmax_x]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetMtilexRiWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtilexBase(cfg) {
  msetModule.io.in.atx := atx
  msetModule.io.in.mtype := oldMtype
  val mtilex = msetModule.io.out.mtilex
  val tilexmax = msetModule.io.out.txmax
  val isMsettilex = MSETtilexOpType.isMsettilex(in.ctrl.fuOpType)

  out.res.data := mtilex

  if (cfg.writeMxRf) io.mtilex.get.bits := msetModule.io.out.mtilex
  if (cfg.writeMxRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMxRf) io.mxIsZero.get := io.out.valid && mtilex === 0.U
  if (cfg.writeMxRf) io.mxIsMxmax.get := io.out.valid && mtilex === tilexmax
}

/**
  * Wrapper of MsetMtilexModule
  * This fu is uop of mset which reads two int regs and writes one vf regs.<br>
  * uop: <br/>
  * [[MSETtilexOpType.uvsetvcfg_vv]], <br/>
  * [[MSETtilexOpType.uvsetvcfg_keep_v]], <br/>
  * @param cfg [[FuConfig]]
  * @param p [[Parameters]]
  */
class MSetMtilexRmfWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtilexBase(cfg) {
  val oldMtilex = in.data.src(0)
  msetModule.io.in.atx := oldMtilex
  msetModule.io.in.mtype := oldMtype

  val mtilex = msetModule.io.out.mtilex
  val txmax = msetModule.io.out.txmax
  val isMsettilex = MSETtilexOpType.isMsettilex(in.ctrl.fuOpType)
  val isMreadMtilex = MSETtilexOpType.isMreadMtilex(in.ctrl.fuOpType)

  // Select output 
  out.res.data := Mux(isMreadMtilex, oldMtilex, mtilex)

  if (cfg.writeMxRf) io.mtilex.get.bits := msetModule.io.out.mtilex
  if (cfg.writeMxRf) io.mtilex.get.valid := io.out.valid && isMsettilex
  if (cfg.writeMxRf) io.mxIsZero.get := io.out.valid && !isMreadMtilex && mtilex === 0.U
  if (cfg.writeMxRf) io.mxIsMxmax.get := io.out.valid && !isMreadMtilex && mtilex === txmax
}

class MSetMtypeBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetMtypeModule: MsetMtypeBaseModule = if (DEV_FIXED_MTYPE) {
    Module(new MsetMtypeDummyModule)
  } else {
    Module(new MsetMtypeModule)
  }

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  protected val newMtypeImm: UInt = Imm_MSET().getAtx(in.data.src(0))
  protected val newMtype: UInt = Mux(MSETtypeOpType.isMsetTypeFromImm(in.ctrl.fuOpType), newMtypeImm, in.data.src(0))

  protected val oldMtype = MType.toMsetMType(io.in.bits.ctrl.mpu.get.mtype)

  msetMtypeModule.io.in.newmtype := newMtype
  msetMtypeModule.io.in.oldmtype := oldMtype
  msetMtypeModule.io.in.func := in.ctrl.fuOpType
  
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  out.res.data := msetMtypeModule.io.out.mtype.asUInt

  if (cfg.writeMType) {
    io.mtype.get.bits := MsetMType.toMType(msetMtypeModule.io.out.mtype)
    io.mtype.get.valid := io.out.valid
  }
}

class MSetMtypeRiWi(cfg: FuConfig)(implicit p: Parameters) extends MSetMtypeBase(cfg) {
  // TODO: Implement MsetMtypeRiWi
}

class MSetMtypeRiWmf(cfg: FuConfig)(implicit p: Parameters) extends MSetMtypeBase(cfg) {
  // TODO: Implement MsetMtypeRiWmf
}