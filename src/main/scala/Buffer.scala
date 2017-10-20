package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import scala.util.Random
import testchipip.StreamIO
import IceNetConsts._

class BufferBRAM(n: Int, w: Int) extends Module {
  val addrBits = log2Ceil(n)
  val io = IO(new Bundle {
    val read = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(addrBits.W))
      val data = Output(Bits(w.W))
    }
    val write = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(addrBits.W))
      val data = Input(Bits(w.W))
    }
  })

  val ram = Mem(n, Bits(w.W))

  val wen = RegNext(io.write.en, false.B)
  val waddr = RegNext(io.write.addr)
  val wdata = RegNext(io.write.data)

  when (wen) { ram.write(waddr, wdata) }

  val rdata = ram.read(io.read.addr)

  io.read.data := RegEnable(rdata, io.read.en)
}

class NetworkPacketBuffer[T <: Data](
    nPackets: Int,
    maxBytes: Int = ETH_MAX_BYTES,
    headerBytes: Int = ETH_HEAD_BYTES,
    headerType: T = new EthernetHeader,
    wordBytes: Int = NET_IF_WIDTH / 8) extends Module {

  val io = IO(new Bundle {
    val stream = new StreamIO(wordBytes * 8)
    val header = Valid(headerType)
  })

  assert(!io.stream.in.valid || io.stream.in.bits.keep.andR,
    "NetworkPacketBuffer does not handle missing data")

  val maxWords = maxBytes / wordBytes
  val headerWords = headerBytes / wordBytes
  val wordBits = wordBytes * 8

  val idxBits = log2Ceil(maxWords + 1)
  val phaseBits = log2Ceil(nPackets)
  val buffers = Seq.fill(nPackets) { Module(new BufferBRAM(maxWords, wordBits)) }
  val headers = Seq.fill(nPackets) { Reg(Vec(headerWords, Bits(wordBits.W))) }
  val bufLengths = Seq.fill(nPackets) { RegInit(0.U(idxBits.W)) }
  val bufValid = Vec(bufLengths.map(len => len > 0.U))

  val inIdx = RegInit(0.U(idxBits.W))
  val inDrop = RegInit(false.B)
  val outIdx = RegInit(0.U(idxBits.W))

  val inPhase = RegInit(0.U(phaseBits.W))
  val outPhase = RegInit(0.U(phaseBits.W))

  val outHeader = Vec(headers.map(
    header => headerType.fromBits(Cat(header.reverse))))
  val outDone = Vec(bufLengths.map(len => outIdx === len))
  val outLast = Vec(bufLengths.map(len => outIdx === (len - 1.U)))
  val outValid = !outDone(outPhase) && bufValid(outPhase)

  val outValidReg = RegInit(false.B)

  val ren = (io.stream.out.ready || !outValidReg) && outValid
  val wen = Wire(init = false.B)
  val hwen = wen && inIdx < headerWords.U

  val outPhaseReg = RegEnable(outPhase, ren)
  val outDataReg = Vec(buffers.map(buffer => buffer.io.read.data))(outPhaseReg)
  val outLastReg = RegEnable(outLast(outPhase), ren)

  io.stream.out.valid := outValidReg
  io.stream.out.bits.data := outDataReg
  io.stream.out.bits.last := outLastReg
  io.stream.in.ready := true.B
  io.header.valid := bufValid(outPhase)
  io.header.bits := outHeader(outPhase)

  when (io.stream.out.fire()) { outValidReg := false.B }
  when (ren) { outValidReg := true.B }

  def wrapInc(x: UInt) = Mux(x === (nPackets - 1).U, 0.U, x + 1.U)

  val bufLenSet = Wire(init = false.B)
  val bufLenClear = Wire(init = false.B)

  for (i <- 0 until nPackets) {
    val buffer = buffers(i)
    val header = headers(i)
    val bufLen = bufLengths(i)

    buffer.io.read.en := ren && outPhase === i.U
    buffer.io.read.addr := outIdx
    buffer.io.write.en := wen && inPhase === i.U
    buffer.io.write.addr := inIdx
    buffer.io.write.data := io.stream.in.bits.data

    when (inPhase === i.U) {
      when (bufLenSet) { bufLen := inIdx + 1.U }
      when (bufLenClear) { bufLen := 0.U }

      when (hwen) { header(inIdx) := io.stream.in.bits.data }
    }
  }

  when (ren) {
    outIdx := outIdx + 1.U
    when (outLast(outPhase)) {
      outIdx := 0.U
      outPhase := wrapInc(outPhase)
      bufLenClear := true.B
    }
  }

  when (io.stream.in.fire()) {
    val startDropping =
      (inPhase === outPhase && bufValid(inPhase)) ||
      (inIdx === maxWords.U)

    when (startDropping) { inDrop := true.B }
    when (!startDropping && !inDrop) {
      wen := true.B
      inIdx := inIdx + 1.U
    }

    when (io.stream.in.bits.last) {
      val nextPhase = wrapInc(inPhase)
      val tooSmall = inIdx < (headerWords - 1).U
      when (!startDropping && !inDrop && !tooSmall) {
        inPhase := nextPhase
        bufLenSet := true.B
      } .otherwise {
        printf("WARNING: dropped packet with %d flits\n", inIdx + 1.U)
      }
      inIdx := 0.U
      inDrop := false.B
    }
  }
}

class NetworkPacketBufferTest extends UnitTest(100000) {
  val buffer = Module(new NetworkPacketBuffer(2, 8, 8, UInt(64.W), 4))

  val rnd = new Random
  val nPackets = 64
  val phaseBits = log2Ceil(nPackets)
  val packetLengths = Vec(Seq.fill(nPackets) { rnd.nextInt(8).U(3.W) })

  val inLFSR = LFSR16(buffer.io.stream.in.fire())
  val outLFSR = LFSR16(buffer.io.stream.out.fire())

  val inCountdown = RegInit(0.U(8.W))
  val outCountdown = RegInit(0.U(8.W))

  val inPhase = RegInit(0.U(phaseBits.W))

  val inIdx = RegInit(0.U(3.W))
  val outIdx = RegInit(0.U(3.W))

  val started = RegInit(false.B)
  val sending = RegInit(false.B)

  buffer.io.stream.in.valid := sending && inCountdown === 0.U
  buffer.io.stream.in.bits.data := inIdx
  buffer.io.stream.in.bits.keep := ~0.U(32.W)
  buffer.io.stream.in.bits.last := inIdx === packetLengths(inPhase)
  buffer.io.stream.out.ready := outCountdown === 0.U

  when (io.start && !started) {
    started := true.B
    sending := true.B
  }

  when (inCountdown > 0.U) { inCountdown := inCountdown - 1.U }
  when (outCountdown > 0.U) { outCountdown := outCountdown - 1.U }

  when (buffer.io.stream.in.fire()) {
    inCountdown := inLFSR >> 8.U
    inIdx := inIdx + 1.U
    when (buffer.io.stream.in.bits.last) {
      inIdx := 0.U
      inPhase := inPhase + 1.U
      when (inPhase === (nPackets - 1).U) { sending := false.B }
    }
  }

  when (buffer.io.stream.out.fire()) {
    outCountdown := outLFSR >> 8.U
    outIdx := outIdx + 1.U
    when (buffer.io.stream.out.bits.last) {
      outIdx := 0.U
    }
  }

  assert(!buffer.io.stream.out.valid || buffer.io.stream.out.bits.data === outIdx,
    "NetworkPacketBufferTest: got output data out of order")
  assert(!buffer.io.header.valid || buffer.io.header.bits === (1L << 32).U,
    "NetworkPacketBufferTest: unexpected header")

  io.finished := started && !sending && !buffer.io.stream.out.valid
}
