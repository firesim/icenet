package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.SerialIO
import IceNetConsts._

class RateLimiter[T <: Data](typ: T, inc: Int, period: Int, size: Int)
    extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
  })

  require(inc >= 1)
  require(period > 1)
  require(size >= inc)

  val tokens = RegInit(size.U(log2Ceil(size + 1).W))
  val inc_timeout = Counter(period).inc()
  val ok_to_send = tokens > 0.U

  io.out.valid := io.in.valid && ok_to_send
  io.in.ready := io.out.ready && ok_to_send
  io.out.bits := io.in.bits

  when (io.in.fire() && inc_timeout) {
    val next_tokens = tokens +& (inc - 1).U
    tokens := Mux(next_tokens > size.U, size.U, next_tokens)
  } .elsewhen (io.in.fire()) {
    tokens := tokens - 1.U
  } .elsewhen (inc_timeout) {
    val next_tokens = tokens +& inc.U
    tokens := Mux(next_tokens > size.U, size.U, next_tokens)
  }
}

object RateLimiter {
  def apply[T <: Data](in: DecoupledIO[T], inc: Int, period: Int, size: Int) = {
    if (period > 1) {
      val limiter = Module(new RateLimiter(
        in.bits.cloneType, inc, period, size))
      limiter.io.in <> Queue(in, size)
      limiter.io.out
    } else { in }
  }
}

class RateLimiterTest extends UnitTest {
  val nFlits = 48
  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val limiter = Module(new RateLimiter(Bool(), 1, 4, 2))
  limiter.io.in.valid := sending
  limiter.io.in.bits := true.B
  limiter.io.out.ready := receiving

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
