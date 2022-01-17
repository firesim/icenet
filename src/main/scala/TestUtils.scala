package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.LatencyPipe
import scala.math.max
import testchipip.{StreamChannel, StreamIO}
import IceNetConsts._

class PacketGen(lengths: Seq[Int], genData: Seq[BigInt]) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val finished = Output(Bool())
    val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
  })

  val maxLength = lengths.reduce(max(_, _))
  val totalLength = lengths.reduce(_ + _)
  val lengthVec = VecInit(lengths.map(_.U))
  val dataVec = VecInit(genData.map(_.U(NET_IF_WIDTH.W)))

  require(totalLength == genData.size)

  val pktIdx = Reg(UInt(log2Ceil(lengths.size).W))
  val pktOffset = Reg(UInt(log2Ceil(maxLength).W))
  val dataIdx = Reg(UInt(log2Ceil(totalLength).W))
  val sending = RegInit(false.B)
  val started = RegInit(false.B)

  when (!sending && io.start) {
    started := true.B
    sending := true.B
    pktIdx := 0.U
    pktOffset := 0.U
    dataIdx := 0.U
  }

  when (io.out.fire) {
    dataIdx := dataIdx + 1.U
    pktOffset := pktOffset + 1.U
    when (io.out.bits.last) {
      pktIdx := pktIdx + 1.U
      pktOffset := 0.U
      when (pktIdx === (lengths.size - 1).U) {
        sending := false.B
      }
    }
  }

  io.out.valid := sending
  io.out.bits.data := dataVec(dataIdx)
  io.out.bits.keep := NET_FULL_KEEP
  io.out.bits.last := pktOffset === lengthVec(pktIdx) - 1.U
  io.finished := started && !sending
}

class PacketCheck(
    checkData: Seq[BigInt],
    checkKeep: Seq[Int],
    checkLast: Seq[Boolean]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val finished = Output(Bool())
  })

  val checkDataVec = VecInit(checkData.map(_.U(NET_IF_WIDTH.W)))
  val checkKeepVec = VecInit(checkKeep.map(_.U(NET_IF_BYTES.W)))
  val checkLastVec = VecInit(checkLast.map(_.B))

  val (checkIdx, checkDone) = Counter(io.in.fire, checkDataVec.length)

  val finished = RegInit(false.B)

  io.in.ready := !finished
  io.finished := finished

  when (checkDone) { finished := true.B }

  def compareData(a: UInt, b: UInt, keep: UInt) = {
    val bitmask = FillInterleaved(8, keep)
    (a & bitmask) === (b & bitmask)
  }

  assert(!io.in.valid ||
    (compareData(io.in.bits.data, checkDataVec(checkIdx), io.in.bits.keep) &&
      io.in.bits.keep === checkKeepVec(checkIdx) &&
      io.in.bits.last === checkLastVec(checkIdx)),
    "PacketCheck: input does not match")

}

class NetDelay(latency: Int) extends Module {
  val io = IO(new Bundle {
    val left = new StreamIO(NET_IF_WIDTH)
    val right = Flipped(new StreamIO(NET_IF_WIDTH))
  })

  io.left.out <> LatencyPipe(io.right.out, latency)
  io.right.in <> LatencyPipe(io.left.in,   latency)
}

object NetDelay {
  def apply(right: StreamIO, latency: Int): StreamIO = {
    val delay = Module(new NetDelay(latency))
    delay.io.right.out <> right.out
    right.in <> delay.io.right.in
    delay.io.left
  }
}
