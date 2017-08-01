package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

class NetworkPacketBuffer(nPackets: Int, maxWords: Int) extends Module {
  val io = IO(new StreamIO(NET_IF_WIDTH))

  val idxBits = log2Ceil(maxWords + 1)
  val phaseBits = log2Ceil(nPackets)
  val buffers = Seq.fill(nPackets) { Mem(maxWords, Bits(NET_IF_WIDTH.W)) }
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

  io.out.valid := !outDone(outPhase) && bufValid(outPhase)
  io.out.bits.data := outData(outPhase)
  io.out.bits.last := outLast(outPhase)
  io.in.ready := true.B

  def wrapInc(x: UInt) = Mux(x === (nPackets - 1).U, 0.U, x + 1.U)

  for (i <- 0 until nPackets) {
    val buffer = buffers(i)
    val bufLen = bufLengths(i)

    when (io.out.fire() && outPhase === i.U) {
      outIdx := outIdx + 1.U
      when (io.out.bits.last) {
        outIdx := 0.U
        outPhase := wrapInc(outPhase)
        bufLen := 0.U
      }
    }

    when (io.in.fire() && inPhase === i.U) {
      val startDropping = inPhase === outPhase && bufValid(i) && inIdx === 0.U

      when (startDropping) { inDrop := true.B }
      when (!startDropping && !inDrop) {
        buffer(inIdx) := io.in.bits.data
      }
      inIdx := inIdx + 1.U

      when (io.in.bits.last) {
        val nextPhase = wrapInc(inPhase)
        when (!inDrop) {
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
  val buffer = Module(new NetworkPacketBuffer(2, 2))

  val inputs = Vec(1.U, 2.U, 3.U, 4.U, 5.U, 6.U)
  val expected = Vec(inputs.take(4))

  val s_start :: s_input :: s_output :: s_done :: Nil = Enum(4)
  val state = RegInit(s_start)

  val (inputIdx, inputDone) = Counter(buffer.io.in.fire(), 6)
  val (outputIdx, outputDone) = Counter(buffer.io.out.fire(), 4)

  when (state === s_start && io.start) { state := s_input }
  when (inputDone) { state := s_output }
  when (outputDone) { state := s_done }

  buffer.io.in.valid := state === s_input
  buffer.io.in.bits.data := inputs(inputIdx)
  buffer.io.in.bits.last := inputIdx(0)
  buffer.io.out.ready := state === s_output

  when (buffer.io.out.fire()) {
    printf("out data: %x\n", buffer.io.out.bits.data)
  }

  assert(
    !buffer.io.out.valid || buffer.io.out.bits.data === expected(outputIdx),
    "Data mismatch")

  assert(!buffer.io.out.valid || buffer.io.out.bits.last === outputIdx(0),
    "Last mismatch")

  io.finished := state === s_done
}
