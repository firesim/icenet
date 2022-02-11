package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

class Aligner(dataBits: Int) extends Module {
  val dataBytes = dataBits / 8

  val io = IO(new StreamIO(dataBits))

  val data = RegInit(0.U(dataBits.W))
  val keep = RegInit(0.U(dataBytes.W))
  val last = RegInit(false.B)
  val nbytes = RegInit(0.U(log2Ceil(dataBytes + 1).W))

  assert(!io.in.valid || io.in.bits.keep.orR,
    "Aligner cannot handle an empty flit")

  val rshift = PriorityEncoder(io.in.bits.keep)
  val full_keep = ((io.in.bits.keep >> rshift) << nbytes) | keep

  val in_mask = FillInterleaved(8, io.in.bits.keep)
  val in_data = io.in.bits.data & in_mask

  val rshift_bit = Cat(rshift, 0.U(3.W))
  val nbits = Cat(nbytes, 0.U(3.W))
  val bitmask = FillInterleaved(8, keep)
  val full_data = ((in_data >> rshift_bit) << nbits) | (data & bitmask)
  val full_nbytes = PopCount(full_keep)
  val fwd_last = io.in.bits.last && (full_keep >> dataBytes.U) === 0.U

  io.out.valid := (last && nbytes > 0.U) ||
                  (io.in.valid && (fwd_last || full_nbytes >= dataBytes.U))
  io.out.bits.data := Mux(last, data, full_data(dataBits-1, 0))
  io.out.bits.keep := Mux(last, keep, full_keep(dataBytes-1, 0))
  io.out.bits.last := last || fwd_last

  io.in.ready := full_nbytes < dataBytes.U ||
                 (io.out.ready && !last)

  when (io.in.fire && io.out.fire) {
    data := full_data >> dataBits.U
    keep := full_keep >> dataBytes.U
    last := io.in.bits.last && !fwd_last
    nbytes := Mux(fwd_last, 0.U, full_nbytes - dataBytes.U)
  } .elsewhen (io.in.fire) {
    data := full_data
    keep := full_keep
    last := io.in.bits.last
    nbytes := full_nbytes
  } .elsewhen (io.out.fire) {
    data := 0.U
    keep := 0.U
    last := false.B
    nbytes := 0.U
  }
}

class AlignerTest extends UnitTest {
  val inData = VecInit(
    "h0011223344556677".U,
    "h8899AABBCCDDEEFF".U,
    "h0123456789ABCDEF".U,
    "hFEDCBA9876543210".U)
  val inKeep = VecInit(
    "b11111100".U,
    "b01111000".U,
    "b00001111".U,
    "b11110000".U)
  val inLast = VecInit(false.B, false.B, true.B, true.B)

  val outData = VecInit(
    "hBBCC001122334455".U,
    "h000089ABCDEF99AA".U,
    "h00000000FEDCBA98".U)
  val outKeep = VecInit(
    "b11111111".U,
    "b00111111".U,
    "b00001111".U)
  val outLast = VecInit(false.B, true.B, true.B)

  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val aligner = Module(new Aligner(NET_IF_WIDTH))

  val (inIdx, inDone) = Counter(aligner.io.in.fire, inData.size)
  val (outIdx, outDone) = Counter(aligner.io.out.fire, outData.size)

  aligner.io.in.valid := sending
  aligner.io.in.bits.data := inData(inIdx)
  aligner.io.in.bits.keep := inKeep(inIdx)
  aligner.io.in.bits.last := inLast(inIdx)
  aligner.io.out.ready := receiving

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B
  }
  when (inDone) { sending := false.B }
  when (outDone) { receiving := false.B }

  io.finished := started && !sending && !receiving

  def compareData(a: UInt, b: UInt, keep: UInt) = {
    val bitmask = FillInterleaved(8, keep)
    (a & bitmask) === (b & bitmask)
  }

  assert(!aligner.io.out.valid ||
    (compareData(
      aligner.io.out.bits.data,
      outData(outIdx),
      aligner.io.out.bits.keep) &&
     aligner.io.out.bits.keep === outKeep(outIdx) &&
     aligner.io.out.bits.last === outLast(outIdx)),
   "AlignerTest: output does not match expected")
}
