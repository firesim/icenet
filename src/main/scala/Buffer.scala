package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

class NetworkPacketBuffer[T <: Data](
    nPackets: Int, maxWords: Int = ETH_MAX_WORDS,
    headerWords: Int = ETH_HEAD_WORDS, headerType: T = new EthernetHeader)
      extends Module {

  val io = IO(new Bundle {
    val stream = new StreamIO(NET_IF_WIDTH)
    val header = Valid(headerType)
  })

  val idxBits = log2Ceil(maxWords + 1)
  val phaseBits = log2Ceil(nPackets)
  val buffers = Seq.fill(nPackets) { Mem(maxWords, Bits(NET_IF_WIDTH.W)) }
  val headers = Seq.fill(nPackets) { Reg(Vec(headerWords, Bits(NET_IF_WIDTH.W))) }
  val bufLengths = Seq.fill(nPackets) { RegInit(0.U(idxBits.W)) }
  val bufValid = Vec(bufLengths.map(len => len > 0.U))

  val inIdx = RegInit(0.U(idxBits.W))
  val inDrop = RegInit(false.B)
  val outIdx = RegInit(0.U(idxBits.W))

  val inPhase = RegInit(0.U(phaseBits.W))
  val outPhase = RegInit(0.U(phaseBits.W))

  val outData = Vec(buffers.map(buffer => buffer(outIdx)))
  val outDone = Vec(bufLengths.map(len => outIdx === len))
  val outLast = Vec(bufLengths.map(len => outIdx === (len - 1.U)))
  val outHeader = Vec(headers.map(header => headerType.fromBits(Cat(header.reverse))))

  io.stream.out.valid := !outDone(outPhase) && bufValid(outPhase)
  io.stream.out.bits.data := outData(outPhase)
  io.stream.out.bits.last := outLast(outPhase)
  io.stream.in.ready := true.B
  io.header.valid := bufValid(outPhase)
  io.header.bits := outHeader(outPhase)

  def wrapInc(x: UInt) = Mux(x === (nPackets - 1).U, 0.U, x + 1.U)

  for (i <- 0 until nPackets) {
    val buffer = buffers(i)
    val header = headers(i)
    val bufLen = bufLengths(i)

    when (io.stream.out.fire() && outPhase === i.U) {
      outIdx := outIdx + 1.U
      when (io.stream.out.bits.last) {
        outIdx := 0.U
        outPhase := wrapInc(outPhase)
        bufLen := 0.U
      }
    }

    when (io.stream.in.fire() && inPhase === i.U) {
      val startDropping =
        (inPhase === outPhase && bufValid(i)) ||
        (inIdx === maxWords.U)

      when (startDropping) { inDrop := true.B }
      when (!startDropping && !inDrop) {
        when (inIdx < headerWords.U) {
          header(inIdx) := io.stream.in.bits.data
        }
        buffer(inIdx) := io.stream.in.bits.data
        inIdx := inIdx + 1.U
      }

      when (io.stream.in.bits.last) {
        val nextPhase = wrapInc(inPhase)
        when (!startDropping && !inDrop) {
          inPhase := nextPhase
          bufLen := inIdx + 1.U
        }
        inIdx := 0.U
        inDrop := false.B
      }
    }
  }
}

class NetworkPacketBufferTest extends UnitTest {
  val buffer = Module(new NetworkPacketBuffer(2, 2, 2, UInt(128.W)))

  val inputData = Vec(1.U, 2.U, 3.U, 4.U, 5.U, 6.U, 7.U, 8.U, 9.U)
  val inputLast = Vec(
    false.B, true.B, false.B, false.B, true.B,
    false.B, true.B, false.B, true.B)
  val expectedData = Vec(1.U, 2.U, 6.U, 7.U)
  val expectedLast = Vec(false.B, true.B, false.B, true.B)
  val expectedHead = Vec(Cat(2.U(64.W), 1.U(64.W)), Cat(7.U(64.W), 6.U(64.W)))

  val s_start :: s_input :: s_output :: s_done :: Nil = Enum(4)
  val state = RegInit(s_start)

  val (inputIdx, inputDone) = Counter(buffer.io.stream.in.fire(), inputData.size)
  val (outputIdx, outputDone) = Counter(buffer.io.stream.out.fire(), expectedData.size)

  when (state === s_start && io.start) { state := s_input }
  when (inputDone) { state := s_output }
  when (outputDone) { state := s_done }

  buffer.io.stream.in.valid := state === s_input
  buffer.io.stream.in.bits.data := inputData(inputIdx)
  buffer.io.stream.in.bits.last := inputLast(inputIdx)
  buffer.io.stream.out.ready := state === s_output

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
    !buffer.io.header.valid ||
    buffer.io.header.bits === expectedHead(outputIdx >> 1.U),
    "Header mismatch")

  io.finished := state === s_done
}
