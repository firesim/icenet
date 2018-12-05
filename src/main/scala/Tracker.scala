package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.HellaPeekingArbiter
import freechips.rocketchip.unittest.UnitTest
import testchipip.{StreamIO, ValidStreamIO, StreamChannel}
import IceNetConsts._

case class CreditTrackerParams(
  inCredits: Int = 0,
  outMaxCredits: Int = 255,
  updatePeriod: Int = 255)

class CreditInTracker(params: CreditTrackerParams) extends Module {
  val io = IO(new Bundle {
    val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
    val in_alloc = Input(Bool())
    val in_free  = Input(Bool())
  })

  val updatePeriod = params.updatePeriod
  val inCredits = params.inCredits

  val timeout = RegInit(0.U(log2Ceil(updatePeriod+1).W))
  val unassigned = RegInit(inCredits.U(log2Ceil(inCredits+1).W))
  val assigned = RegInit(0.U(log2Ceil(inCredits+1).W))

  io.out.valid := (unassigned > 0.U) && (assigned === 0.U || timeout === 0.U)
  io.out.bits.data := unassigned
  io.out.bits.keep := NET_FULL_KEEP
  io.out.bits.last := true.B

  when (io.out.fire()) {
    timeout := updatePeriod.U
  } .elsewhen (timeout > 0.U) {
    timeout := timeout - 1.U
  }

  unassigned := (unassigned - Mux(io.out.fire(), unassigned, 0.U)) +
                  Mux(io.in_free, 1.U, 0.U)
  assigned := (assigned + Mux(io.out.fire(), unassigned, 0.U)) -
                  Mux(io.in_alloc, 1.U, 0.U)
}

class CreditOutTracker(params: CreditTrackerParams) extends Module {
  val creditBits = log2Ceil(params.outMaxCredits+1)
  val io = IO(new Bundle {
    val ext = new StreamIO(NET_IF_WIDTH)
    val int = Flipped(new StreamIO(NET_IF_WIDTH))
  })

  val credits = RegInit(0.U(creditBits.W))
  val canForward = credits > 0.U

  val first = RegInit(true.B)
  val updateData = io.ext.in.bits.data(creditBits-1, 0)
  val isUpdate = first && updateData =/= 0.U
  val isUpdateReg = RegInit(false.B)

  when (io.ext.in.fire()) {
    first := false.B
    when (isUpdate) { isUpdateReg := true.B }
    when (io.ext.in.bits.last) {
      first := true.B
      isUpdateReg := false.B
    }
  }

  io.int.in.valid := io.ext.in.valid && !isUpdate
  io.int.in.bits  := io.ext.in.bits
  io.ext.in.ready := isUpdate || isUpdateReg || io.int.in.ready

  credits := (credits + Mux(io.ext.in.fire() && isUpdate, updateData, 0.U)) -
                        Mux(io.ext.out.fire() && io.ext.out.bits.last, 1.U, 0.U)

  io.ext.out.valid := io.int.out.valid && canForward
  io.ext.out.bits  := io.int.out.bits
  io.int.out.ready := io.ext.out.ready && canForward
}

class CreditTracker(params: CreditTrackerParams) extends Module {
  val io = IO(new Bundle {
    val ext = new StreamIO(NET_IF_WIDTH)
    val int = Flipped(new StreamIO(NET_IF_WIDTH))
    val in_free = Input(Bool())
  })

  val outTrack = Module(new CreditOutTracker(params))
  val inTrack = Module(new CreditInTracker(params))

  outTrack.io.ext.in <> io.ext.in
  io.int.in <> outTrack.io.int.in
  outTrack.io.int.out <> io.int.out
  inTrack.io.in_alloc := io.int.in.fire() && io.int.in.bits.last
  inTrack.io.in_free  := io.in_free

  val extOutArb = Module(new HellaPeekingArbiter(
    new StreamChannel(NET_IF_WIDTH), 2, (ch: StreamChannel) => ch.last))
  extOutArb.io.in <> Seq(inTrack.io.out, outTrack.io.ext.out)
  io.ext.out <> extOutArb.io.out
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

class CreditTrackerTest extends UnitTest {
  val s_idle :: s_send :: s_wait :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // Make sure leading numbers are all even
  val testData = Vec(Seq(0, 5, 7, 28 << 16, 11, 34 << 16).map(_.U(NET_IF_WIDTH.W)))
  val testLast = Vec(Seq(false, false, true, false, true, true).map(_.B))

  val tracker = Module(new CreditTracker(CreditTrackerParams(2)))
  val queue = Module(new DelayQueue(new StreamChannel(NET_IF_WIDTH), 10))

  val (outIdx, outDone) = Counter(tracker.io.int.out.fire(), testData.size)
  val (inIdx, inDone) = Counter(queue.io.deq.fire(), testData.size)

  tracker.io.ext.in <> tracker.io.ext.out
  tracker.io.in_free := queue.io.deq.fire() && queue.io.deq.bits.last
  tracker.io.int.out.valid := state === s_send
  tracker.io.int.out.bits.data := testData(outIdx)
  tracker.io.int.out.bits.last := testLast(outIdx)
  tracker.io.int.out.bits.keep := NET_FULL_KEEP
  queue.io.enq <> tracker.io.int.in
  queue.io.deq.ready := true.B

  assert(!queue.io.deq.valid ||
    (queue.io.deq.bits.data === testData(inIdx) &&
     queue.io.deq.bits.last === testLast(inIdx)),
   "CreditTrackerTest: data or last does not match")

  assert(!tracker.io.ext.out.valid || tracker.io.ext.in.ready,
    "External input was back-pressured")

  when (state === s_idle && io.start) { state := s_send }
  when (outDone) { state := s_wait }
  when (inDone) { state := s_done }

  io.finished := state === s_done
}
