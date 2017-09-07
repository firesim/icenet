package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
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
      when (!startDropping && !inDrop) {
        inPhase := nextPhase
        bufLenSet := true.B
      }
      inIdx := 0.U
      inDrop := false.B
    }
  }
}

class NetworkPacketBufferTest extends UnitTest {
  val buffer = Module(new NetworkPacketBuffer(2, 8, 8, UInt(64.W), 4))

  val inputData = Vec(1.U, 2.U, 3.U, 4.U, 5.U, 6.U, 7.U, 8.U, 9.U)
  val inputLast = Vec(
    false.B, true.B, false.B, false.B, true.B,
    false.B, true.B, false.B, true.B)
  val expectedData = Vec(1.U, 2.U, 6.U, 7.U)
  val expectedLast = Vec(false.B, true.B, false.B, true.B)
  val expectedHead = Vec(Cat(2.U(32.W), 1.U(32.W)), Cat(7.U(32.W), 6.U(32.W)))

  val s_start :: s_input :: s_output :: s_done :: Nil = Enum(4)
  val state = RegInit(s_start)

  val delayDone = Counter(10).inc()
  val (inputIdx, inputDone) = Counter(buffer.io.stream.in.fire(), inputData.size)
  val (outputIdx, outputDone) = Counter(buffer.io.stream.out.fire(), expectedData.size)

  when (state === s_start && io.start) { state := s_input }
  when (inputDone) { state := s_output }
  when (outputDone) { state := s_done }

  buffer.io.stream.in.valid := state === s_input && delayDone
  buffer.io.stream.in.bits.keep := ~0.U(4.W)
  buffer.io.stream.in.bits.data := inputData(inputIdx)
  buffer.io.stream.in.bits.last := inputLast(inputIdx)
  buffer.io.stream.out.ready := state === s_output && delayDone

  when (buffer.io.stream.out.fire()) {
    printf("out data: %x\n", buffer.io.stream.out.bits.data)
  }

  assert(
    !buffer.io.stream.out.valid ||
    buffer.io.stream.out.bits.data === expectedData(outputIdx),
    "Data mismatch")

  assert(
    !buffer.io.stream.out.valid ||
    buffer.io.stream.out.bits.last === expectedLast(outputIdx),
    "Last mismatch")

  assert(
    !buffer.io.header.valid || outputIdx(0) ||
    buffer.io.header.bits === expectedHead(outputIdx >> 1.U),
    "Header mismatch")

  io.finished := state === s_done
}
