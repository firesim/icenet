package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.{StreamIO, ValidStreamIO, StreamChannel}
import IceNetConsts._

class CreditTracker(inCredits: Int, netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle {
    val ext = new ValidStreamIO(netConfig.NET_IF_WIDTH_BITS)
    val int = Flipped(new StreamIO(netConfig.NET_IF_WIDTH_BITS))
    val in_free = Input(Bool())
  })

  val in_busy = RegInit(false.B)
  val is_credit_mess = io.ext.in.bits.data(0) && !in_busy
  val in_credits = io.ext.in.bits.data >> 1.U
  val in_space_pending = RegInit(inCredits.U)
  val out_space = RegInit(0.U)
  val out_busy = RegInit(false.B)

  when (io.int.in.fire()) {
    when (!in_busy) { in_busy := true.B }
    when (io.int.in.bits.last) { in_busy := false.B }
  }

  when (io.int.out.fire()) {
    when (!out_busy) { out_busy := true.B }
    when (io.int.out.bits.last) { out_busy := false.B }
  }

  val send_credit_mess = in_space_pending > 0.U && !out_busy

  in_space_pending := in_space_pending +
    Mux(io.in_free, 1.U, 0.U) -
    Mux(send_credit_mess, in_space_pending, 0.U)

  out_space := out_space +
    Mux(io.ext.in.valid && is_credit_mess, in_credits, 0.U) -
    Mux(io.int.in.fire() && io.int.in.bits.last, 1.U, 0.U)

  io.int.in.valid := io.ext.in.valid && !is_credit_mess
  io.int.in.bits := io.ext.in.bits
  io.int.out.ready := out_space > 0.U && !send_credit_mess
  io.ext.out.valid := send_credit_mess || io.int.out.fire()
  io.ext.out.bits.data := Mux(send_credit_mess,
    Cat(in_space_pending, true.B), io.int.out.bits.data)
  io.ext.out.bits.keep := DontCare
  io.ext.out.bits.last := send_credit_mess || io.int.out.bits.last

  assert(!io.int.in.valid || io.int.in.ready,
    "CreditTracker: internal input not ready")
}

class DelayQueue[T <: Data](typ: T, stages: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(typ))
    val deq = Decoupled(typ)
  })

  val queues = Seq.fill(stages) { Module(new Queue(typ, 1, pipe=true)) }

  queues.head.io.enq <> io.enq
  queues.init.zip(queues.tail).foreach { case (upper, lower) =>
    lower.io.enq <> upper.io.deq
  }
  io.deq <> queues.last.io.deq
}

class CreditTrackerTest(netIfWidth: Int = 64) extends UnitTest {

  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = netIfWidth)

  val s_idle :: s_send :: s_wait :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // Make sure leading numbers are all even
  val testData = Vec(Seq(0, 5, 7, 28, 11, 34).map(_.U(netConfig.NET_IF_WIDTH_BITS.W)))
  val testLast = Vec(Seq(false, false, true, false, true, true).map(_.B))

  val tracker = Module(new CreditTracker(1, netConfig))
  val queue = Module(new DelayQueue(new StreamChannel(netConfig.NET_IF_WIDTH_BITS), 5))

  val (outIdx, outDone) = Counter(tracker.io.int.out.fire(), testData.size)
  val (inIdx, inDone) = Counter(queue.io.deq.fire(), testData.size)

  tracker.io.ext.in <> tracker.io.ext.out
  tracker.io.in_free := queue.io.deq.fire() && queue.io.deq.bits.last
  tracker.io.int.out.valid := state === s_send
  tracker.io.int.out.bits.data := testData(outIdx)
  tracker.io.int.out.bits.keep := DontCare
  tracker.io.int.out.bits.last := testLast(outIdx)
  queue.io.enq <> tracker.io.int.in
  queue.io.deq.ready := true.B

  assert(!queue.io.deq.valid ||
    (queue.io.deq.bits.data === testData(inIdx) &&
     queue.io.deq.bits.last === testLast(inIdx)),
   "CreditTrackerTest: data or last does not match")

  when (state === s_idle && io.start) { state := s_send }
  when (outDone) { state := s_wait }
  when (inDone) { state := s_done }

  io.finished := state === s_done
}
