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

class MSetBase(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  val debugIO = IO(new Bundle() {
    val mconfig = Output(MConfig())
  })
  protected val in = io.in.bits
  protected val out = io.out.bits

  protected val msetModule = Module(new MsetModule)

  protected val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  protected val atxImm = Imm_MSET().getAtx(in.data.src(1))
  protected val atx = Mux(MatrixSETOpType.isMsettilexi(in.ctrl.fuOpType), atxImm, in.data.src(0))

  // FIXME: Different from V extension, matrix extension set mtype and atm/n/k separately.
  //        We temporarily use empty mtype for atm/n/k. This is obviously wrong.
  protected val mtype = 0.U.asTypeOf(new MtypeStruct)

  // FIXME: Moreover, atm/n/k are set separately. This is also wrong.
  //        Just a placeholder.
  msetModule.io.in.atm := atx
  msetModule.io.in.atn := atx
  msetModule.io.in.atk := atx
  msetModule.io.in.mtype := mtype

  msetModule.io.in.func := in.ctrl.fuOpType
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

// class VSetRiWi(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
//   vsetModule.io.in.avl := avl
//   vsetModule.io.in.vtype := vtype

//   out.res.data := vsetModule.io.out.vconfig.vl

//   debugIO.vconfig := vsetModule.io.out.vconfig
// }

// class VSetRiWvf(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
//   vsetModule.io.in.avl := avl
//   vsetModule.io.in.vtype := vtype
//   val vl = vsetModule.io.out.vconfig.vl
//   val vlmax = vsetModule.io.out.vlmax
//   val isVsetvl = VSETOpType.isVsetvl(in.ctrl.fuOpType)

//   out.res.data := vl

//   if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
//   if (cfg.writeVlRf) io.vtype.get.valid := io.out.valid && isVsetvl
//   if (cfg.writeVlRf) io.vlIsZero.get := io.out.valid && vl === 0.U
//   if (cfg.writeVlRf) io.vlIsVlmax.get := io.out.valid && vl === vlmax

//   debugIO.vconfig := vsetModule.io.out.vconfig
// }

// class VSetRvfWvf(cfg: FuConfig)(implicit p: Parameters) extends VSetBase(cfg) {
//   val oldVL = in.data.src(4).asTypeOf(VConfig()).vl
//   vsetModule.io.in.avl := oldVL
//   vsetModule.io.in.vtype := vtype

//   val vl = vsetModule.io.out.vconfig.vl
//   val vlmax = vsetModule.io.out.vlmax
//   val isVsetvl = VSETOpType.isVsetvl(in.ctrl.fuOpType)
//   val isReadVl = in.ctrl.fuOpType === VSETOpType.csrrvl

//   // csrr vl instruction will use this exu to read vl
//   out.res.data := Mux(isReadVl, oldVL, vl)

//   if (cfg.writeVlRf) io.vtype.get.bits := vsetModule.io.out.vconfig.vtype
//   if (cfg.writeVlRf) io.vtype.get.valid := isVsetvl && io.out.valid
//   if (cfg.writeVlRf) io.vlIsZero.get := io.out.valid && !isReadVl && vl === 0.U
//   if (cfg.writeVlRf) io.vlIsVlmax.get := io.out.valid && !isReadVl && vl === vlmax

//   debugIO.vconfig := vsetModule.io.out.vconfig
// }