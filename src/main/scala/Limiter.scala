package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.SerialIO
import IceNetConsts._

/**
 * This specifies the particular settings to limit the bandwidth of the NIC.
 * Note: Since this is parameterized, you have to set the fields correctly in software
 *       (Cannot be a static assignment with a MMIO reg)
 */
class RateLimiterSettings(netConfig: IceNetConfig) extends Bundle {
  val incBits = log2Ceil(netConfig.RLIMIT_MAX_INC)
  val periodBits = log2Ceil(netConfig.RLIMIT_MAX_PERIOD)
  val sizeBits = log2Ceil(netConfig.RLIMIT_MAX_SIZE)

  /*
   * Given a clock frequency of X, you can achieve an output bandwidth
   * of Y = X * (N / D), where N <= D, by setting inc to N and period to (D - 1).
   * The field size should be set to the number of consecutive beats that
   * can be sent before rate-limiting kicks in.
   */
  val inc = UInt(incBits.W)
  val period = UInt(periodBits.W)
  val size = UInt(sizeBits.W)

  override def cloneType = (new RateLimiterSettings(netConfig)).asInstanceOf[this.type]
}

/**
 * Functional block that throttles the bandwidth of the NIC. Uses a counter that is decremented
 * everytime a flit is sent. This counter is incremented by k every p cycles providing a bandwidth
 * of k/p * normal rate.
 *
 * @param typ type of data to be limited
 */
class RateLimiter[T <: Data](typ: T, netConfig: IceNetConfig) extends Module {
  val incBits = log2Ceil(netConfig.RLIMIT_MAX_INC)
  val periodBits = log2Ceil(netConfig.RLIMIT_MAX_PERIOD)
  val sizeBits = log2Ceil(netConfig.RLIMIT_MAX_SIZE)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
    val settings = Input(new RateLimiterSettings(netConfig))
  })

  val tokens = RegInit(0.U(sizeBits.W))
  val counter = RegInit(0.U(periodBits.W))

  when (counter === 0.U) {
    counter := io.settings.period
  }
  .otherwise {
    counter := counter - 1.U
  }

  val inc_trigger = counter === 0.U
  val ok_to_send = tokens > 0.U

  io.in.ready := io.out.ready && ok_to_send
  io.out.valid := io.in.valid && ok_to_send
  io.out.bits := io.in.bits

  def uint_min(a: UInt, b: UInt) = Mux(a < b, a, b)

  when (inc_trigger && io.out.fire()) {
    val next_tokens = tokens +& (io.settings.inc - 1.U)
    tokens := uint_min(next_tokens, io.settings.size)
  }
  .elsewhen (inc_trigger) {
    val next_tokens = tokens +& io.settings.inc
    tokens := uint_min(next_tokens, io.settings.size)
  }
  .elsewhen (io.out.fire()) {
    tokens := tokens - 1.U
  }
}

/**
 * Companion object to RateLimiter class
 */
object RateLimiter {
  def apply[T <: Data](in: DecoupledIO[T], inc: Int, period: Int, size: Int, netConfig: IceNetConfig) = {
    if (period > 1) {
      val limiter = Module(new RateLimiter(in.bits.cloneType, netConfig))
      limiter.io.in <> in
      limiter.io.settings.inc := inc.U
      limiter.io.settings.period := (period - 1).U
      limiter.io.settings.size := size.U
      limiter.io.out
    }
    else { in }
  }
}

/**
 * Unit test for RateLimiter class
 *
 * @param testWidth size of flit
 */
class RateLimiterTest(testWidth: Int = 64) extends UnitTest {
  val nFlits = 48
  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = testWidth)
  val limiter = Module(new RateLimiter(Bool(), netConfig))
  limiter.io.in.valid := sending
  limiter.io.in.bits := true.B
  limiter.io.out.ready := receiving
  limiter.io.settings.inc := 1.U
  limiter.io.settings.period := 3.U
  limiter.io.settings.size := 2.U

  val (sendCount, sendDone) = Counter(limiter.io.in.fire(), nFlits)
  val (recvCount, recvDone) = Counter(limiter.io.out.fire(), nFlits)

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B
  }

  when (sendDone) { sending := false.B }
  when (recvDone) { receiving := false.B }

  io.finished := started && !sending && !receiving
}
