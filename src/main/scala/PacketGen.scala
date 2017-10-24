package icenet

import chisel3._
import chisel3.util._
import scala.math.max
import testchipip.StreamChannel
import IceNetConsts._

class PacketGen(lengths: Seq[Int], genData: Seq[Long]) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
  })

  val maxLength = lengths.reduce(max(_, _))
  val totalLength = lengths.reduce(_ + _)
  val lengthVec = Vec(lengths.map(_.U))
  val dataVec = Vec(genData.map(_.U(NET_IF_WIDTH.W)))

  require(totalLength == genData.size)

  val pktIdx = Reg(UInt(log2Ceil(lengths.size).W))
  val pktOffset = Reg(UInt(log2Ceil(maxLength).W))
  val dataIdx = Reg(UInt(log2Ceil(totalLength).W))
  val sending = RegInit(false.B)

  when (!sending && io.start) {
    sending := true.B
    pktIdx := 0.U
    pktOffset := 0.U
    dataIdx := 0.U
  }

  when (io.out.fire()) {
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
}
