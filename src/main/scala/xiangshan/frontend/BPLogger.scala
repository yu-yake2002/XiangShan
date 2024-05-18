package xiangshan.frontend

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, HasCLikeTypes, TLLog}

class BPReqLog extends Bundle with HasCLikeTypes {
  val pc = uint64_t
  val bankhit = uint8_t
  val fifohit = uint8_t
}

class BPUpdLog extends Bundle with HasCLikeTypes {
  val pc = uint64_t
}

class BPReqLogger(name: String)(implicit p: Parameters) {
  val io = IO(new Bundle {
    val reqLog = ValidIO(new BPReqLog)
  })
}

class BPReqNullLogger() {
  val io = IO(new Bundle {
    val reqLog = ValidIO(new BPReqLog)
  })
}

object BPReqLogger {
  val table = ChiselDB.createTable("BPLog", new BPReqLog, basicDB = true)

  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    if (enable) {
      val logger = new BPReqLogger(name)
      logger.io.reqLog
    } else {
      val logger = new BPReqNullLogger
      logger.io.reqLog
    }
  }
}

class BPUpdLogger(name: String)(implicit p: Parameters) {
  val io = IO(new Bundle {
    val updLog = ValidIO(new BPUpdLog)
  })
}

class BPUpdNullLogger() {
  val io = IO(new Bundle {
    val updLog = ValidIO(new BPUpdLog)
  })
}

object BPUpdLogger {
  val table = ChiselDB.createTable("BPLog", new BPUpdLog, basicDB = true)

  def apply(name: String, enable: Boolean = true)(implicit p: Parameters) = {
    if (enable) {
      val logger = new BPUpdLogger(name)
      logger.io.updLog
    } else {
      val logger = new BPUpdNullLogger
      logger.io.updLog
    }
  }
}
