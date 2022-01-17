package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.util.TwoWayCounter
import scala.util.Random
import testchipip.{StreamIO, StreamChannel}
import IceNetConsts._

class BufferBRAM[T <: Data](n: Int, typ: T) extends Module {
  val addrBits = log2Ceil(n)
  val io = IO(new Bundle {
    // The value in data becomes valid one cycle after enable is asserted.
    // The value is held so long as enable is false
    val read = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(addrBits.W))
      val data = Output(typ)
    }
    val write = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(addrBits.W))
      val data = Input(typ)
    }
  })

  val ram = Mem(n, typ)

  val wen = RegNext(io.write.en, false.B)
  val waddr = RegNext(io.write.addr)
  val wdata = RegNext(io.write.data)

  when (wen) { ram.write(waddr, wdata) }

  val rread_data = RegEnable(ram.read(io.read.addr), io.read.en)
  val rbypass = RegEnable(io.read.addr === waddr && wen, io.read.en)
  val rbypass_data = RegEnable(wdata, io.read.en)

  io.read.data := Mux(rbypass, rbypass_data, rread_data)
}

/**
 * Buffers incoming packets without backpressuring
 * Drops at packet boundaries if buffer fills us.
 * @bufWords Number of flits held by the buffer
 * @maxBytes Maximum number of bytes in a packet
 * @headerBytes Number of bytes in a header
 * @headerType Bundle type for header
 * @wordBytes Number of bytes in a flit
 * @dropChecks Sequence of functions that can trigger a drop by asserting Bool
 * @dropless If true, dropped packet causes assertion failure
 */
class NetworkPacketBuffer[T <: Data](
    bufWords: Int,
    maxBytes: Int = ETH_JUMBO_MAX_BYTES,
    headerBytes: Int = ETH_HEAD_BYTES,
    headerType: T = new EthernetHeader,
    wordBytes: Int = NET_IF_WIDTH / 8,
    dropChecks: Seq[(T, StreamChannel, Bool) => Bool] = Nil,
    dropless: Boolean = false) extends Module {

  val maxWords = (maxBytes - 1) / wordBytes + 1
  val headerWords = headerBytes / wordBytes
  val wordBits = wordBytes * 8
  val nPackets = (bufWords - 1) / (headerWords + 1) + 1

  require(headerBytes % wordBytes == 0)

  val idxBits = log2Ceil(bufWords)
  val phaseBits = log2Ceil(nPackets + 1)
  val lenBits = log2Ceil(maxBytes + 1)
  val byteOffset = log2Ceil(wordBytes)

  val io = IO(new Bundle {
    val stream = new StreamIO(wordBytes * 8)
    val length = Valid(UInt(lenBits.W))
    val count = Output(UInt(log2Ceil(nPackets+1).W))
    val free = Output(UInt(8.W))
  })

  def discontinuous(bits: UInt, w: Int): Bool =
    (1 until w).map(i => bits(i) && !bits(i-1)).reduce(_ || _)

  assert(!io.stream.in.valid ||
    Mux(io.stream.in.bits.last,
      !discontinuous(io.stream.in.bits.keep, wordBytes),
      io.stream.in.bits.keep.andR),
    "NetworkPacketBuffer does not handle missing data")

  val dataBuffer = Module(new BufferBRAM(bufWords, Bits(wordBits.W)))
  val headerVec = Reg(Vec(headerWords, Bits(wordBits.W)))
  val header = headerVec.asTypeOf(headerType)

  val lenBuffer = Module(new BufferBRAM(nPackets, UInt(lenBits.W)))

  val bufHead = RegInit(0.U(idxBits.W))
  val bufTail = RegInit(0.U(idxBits.W))
  val startHead = Reg(UInt(idxBits.W))
  val revertHead = WireInit(false.B)

  val maybeFull = RegInit(false.B)
  val ptrMatch = bufHead === bufTail
  val bufFull = maybeFull && ptrMatch
  val bufEmpty = !maybeFull && ptrMatch

  val inIdx = RegInit(0.U(idxBits.W))
  val inDrop = RegInit(false.B)
  val inLen = RegInit(0.U(lenBits.W))
  val outIdx = RegInit(0.U(idxBits.W))

  val inPhase = RegInit(0.U(phaseBits.W))
  val outPhase = RegInit(0.U(phaseBits.W))
  val pktCount = RegInit(0.U(phaseBits.W))
  val nextLen = inLen + PopCount(io.stream.in.bits.keep)
  val hasPackets = pktCount > 0.U

  assert(pktCount <= nPackets.U, "Accepted more packets than possible")

  def bytesToWords(nbytes: UInt): UInt =
    nbytes(lenBits-1, byteOffset) + nbytes(byteOffset-1, 0).orR

  def finalKeep(nbytes: UInt): UInt = {
    val remBytes = nbytes(byteOffset-1, 0)
    val finalBytes = Mux(remBytes.orR, remBytes, wordBytes.U)
    (1.U << finalBytes) - 1.U
  }

  val length = lenBuffer.io.read.data
  val numWords = bytesToWords(length)
  // Need one cycle to read out packet length
  val lengthKnown = RegInit(false.B)
  val readLen = !lengthKnown && hasPackets

  val outLast = outIdx === (numWords - 1.U)
  val outValidReg = RegInit(false.B)

  val ren = (io.stream.out.ready || !outValidReg) && lengthKnown && !bufEmpty
  val wen = WireInit(false.B)
  val hwen = wen && inIdx < headerWords.U

  val setLength = WireInit(false.B)
  val clearLength = ren && outLast

  pktCount := pktCount + setLength - clearLength

  val outLastReg = RegEnable(outLast, ren)
  val outIdxReg = RegEnable(outIdx, ren)

  io.stream.out.valid := outValidReg
  io.stream.out.bits.data := dataBuffer.io.read.data
  io.stream.out.bits.last := outLastReg
  io.stream.out.bits.keep := Mux(outLastReg, finalKeep(length), ~0.U(wordBytes.W))
  io.stream.in.ready := true.B
  io.length.bits := lenBuffer.io.read.data
  io.length.valid := lengthKnown
  io.count := pktCount

  def wrapInc(x: UInt, n: Int) = Mux(x === (n - 1).U, 0.U, x + 1.U)

  dataBuffer.io.read.en := ren
  dataBuffer.io.read.addr := bufTail
  dataBuffer.io.write.en := wen
  dataBuffer.io.write.addr := bufHead
  dataBuffer.io.write.data := io.stream.in.bits.data

  lenBuffer.io.read.en := readLen
  lenBuffer.io.read.addr := outPhase
  lenBuffer.io.write.en := setLength
  lenBuffer.io.write.addr := inPhase
  lenBuffer.io.write.data := nextLen

  val headerValid = inIdx >= headerWords.U
  val customDrop = dropChecks.map(check => check(
                                    header,
                                    io.stream.in.bits,
                                    io.stream.in.fire && headerValid))
                             .foldLeft(false.B)(_ || _)
  val startDropping =
    (inPhase === outPhase && hasPackets) ||
    (inIdx === maxWords.U) || bufFull

  val capDrop = startDropping || inDrop
  val nonCapDrop = !headerValid || customDrop
  val anyDrop = capDrop || nonCapDrop
  val dropLastFire = anyDrop && io.stream.in.fire && io.stream.in.bits.last

  // io.free indicates the number of flits being freed up on a cycle
  io.free := ren + Mux(dropLastFire, inIdx + 1.U, 0.U)

  when (io.stream.out.fire) { outValidReg := false.B }

  when (readLen) { lengthKnown := true.B }

  when (ren) {
    outValidReg := true.B
    bufTail := wrapInc(bufTail, bufWords)
    outIdx := outIdx + 1.U

    when (outLast) {
      outIdx := 0.U
      outPhase := wrapInc(outPhase, nPackets)
      lengthKnown := false.B
    }
  }

  when (hwen) { headerVec(inIdx) := io.stream.in.bits.data }

  when (wen) { bufHead := wrapInc(bufHead, bufWords) }

  when (ren =/= wen) { maybeFull := wen }

  when (io.stream.in.fire) {
    when (inIdx === 0.U) { startHead := bufHead }
    when (startDropping) { inDrop := true.B }
    wen := !startDropping && !inDrop
    when (inIdx =/= (bufWords - 1).U) { inIdx := inIdx + 1.U }

    inLen := nextLen

    when (io.stream.in.bits.last) {
      val nextPhase = wrapInc(inPhase, nPackets)
      // Drop packets if there aren't more than headerBytes amount of data
      when (!anyDrop) {
        setLength := true.B
        inPhase := nextPhase
      } .otherwise {
        wen := false.B
        revertHead := inIdx =/= 0.U
        if (dropless) {
          assert(!capDrop,
            "Packet dropped by buffer due to insufficient capacity")
        } else {
          printf("WARNING: dropped packet with %d flits\n", inIdx + 1.U)
        }
      }
      inIdx := 0.U
      inLen := 0.U
      inDrop := false.B
    }
  }

  when (revertHead) {
    bufHead := startHead
    maybeFull := false.B
  }
}

class NetworkPacketBufferTest extends UnitTest(100000) {
  val buffer = Module(new NetworkPacketBuffer(
    bufWords = 12,
    maxBytes = 32,
    headerBytes = 8,
    headerType = UInt(64.W),
    wordBytes = 4))

  val inPackets = Seq(
    (10, false), // drop because too long
    (4,  true),
    (1,  false), // drop because too short
    (5,  false),
    (7,  true),
    (8,  false),
    (6,  false), // drop because buffer full
    (4,  true),
    (3,  false),
    (5,  true),
    (6,  false),
    (4,  true),
    (5,  true))

  val outPackets = Seq(
    (4, true),
    (5, false),
    (7, true),
    (8, false),
    (4, true),
    (3, false),
    (5, true),
    (6, true),
    (4, false),
    (5, true))

  val phaseBits = log2Ceil(inPackets.length)
  val idxBits = 4

  val inPacketLengths = VecInit(inPackets.map {
    case (x, _) => (x - 1).U(idxBits.W)
  })
  val inPacketSwitch = VecInit(inPackets.map(_._2.B))

  val outPacketLengths = VecInit(outPackets.map {
    case (x, _) => (x - 1).U(idxBits.W)
  })
  val outPacketSwitch = VecInit(outPackets.map(_._2.B))

  val inPhase = RegInit(0.U(phaseBits.W))
  val outPhase = RegInit(0.U(phaseBits.W))

  val inIdx = RegInit(0.U(idxBits.W))
  val outIdx = RegInit(0.U(idxBits.W))

  val s_start :: s_input :: s_output :: s_done :: Nil = Enum(4)
  val state = RegInit(s_start)

  buffer.io.stream.in.valid := state === s_input
  buffer.io.stream.in.bits.data := inIdx
  buffer.io.stream.in.bits.keep := ~0.U(32.W)
  buffer.io.stream.in.bits.last := inIdx === inPacketLengths(inPhase)
  buffer.io.stream.out.ready := state === s_output

  when (io.start && state === s_start) {
    state := s_input
  }

  when (buffer.io.stream.in.fire) {
    inIdx := inIdx + 1.U
    when (buffer.io.stream.in.bits.last) {
      inIdx := 0.U
      inPhase := inPhase + 1.U
      when (inPacketSwitch(inPhase)) { state := s_output }
    }
  }

  when (buffer.io.stream.out.fire) {
    outIdx := outIdx + 1.U
    when (buffer.io.stream.out.bits.last) {
      outIdx := 0.U
      outPhase := outPhase + 1.U
      when (outPacketSwitch(outPhase)) { state := s_input }
      when (outPhase === (outPackets.length - 1).U) { state := s_done }
    }
  }

  assert(!buffer.io.stream.out.valid || buffer.io.stream.out.bits.data === outIdx,
    "NetworkPacketBufferTest: got wrong output data")
  assert(!buffer.io.stream.out.valid || !buffer.io.stream.out.bits.last ||
    outIdx === outPacketLengths(outPhase),
    "NetworkPacketBufferTest: got output packet with wrong length")

  io.finished := state === s_done
}

class ReservationBufferAlloc(nXacts: Int, nWords: Int) extends Bundle {
  private val xactIdBits = log2Ceil(nXacts)
  private val countBits = log2Ceil(nWords + 1)

  val id = UInt(xactIdBits.W)
  val count = UInt(countBits.W)

}

class ReservationBufferData(nXacts: Int, dataBits: Int) extends Bundle {
  private val xactIdBits = log2Ceil(nXacts)

  val id = UInt(xactIdBits.W)
  val data = new StreamChannel(dataBits)

}

class ReservationBuffer(nXacts: Int, nWords: Int, dataBits: Int) extends Module {
  private val xactIdBits = log2Ceil(nXacts)
  private val countBits = log2Ceil(nWords + 1)

  require(nXacts <= nWords)

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(new ReservationBufferAlloc(nXacts, nWords)))
    val in = Flipped(Decoupled(new ReservationBufferData(nXacts, dataBits)))
    val out = Decoupled(new StreamChannel(dataBits))
  })

  def incWrap(cur: UInt, inc: UInt): UInt = {
    val unwrapped = cur +& inc
    Mux(unwrapped >= nWords.U, unwrapped - nWords.U, unwrapped)
  }

  val buffer = Module(new BufferBRAM(nWords, new StreamChannel(dataBits)))
  val bufValid = RegInit(0.U(nWords.W))

  val head = RegInit(0.U(countBits.W))
  val tail = RegInit(0.U(countBits.W))
  val count = RegInit(0.U(countBits.W))

  val full = (count + io.alloc.bits.count) > nWords.U
  val xactHeads = Reg(Vec(nXacts, UInt(countBits.W)))
  val curXactHead = xactHeads(io.in.bits.id)

  val occupied = RegInit(false.B)
  val ren = (!occupied || io.out.ready) && (bufValid >> tail)(0)

  count := count +
              Mux(io.alloc.fire, io.alloc.bits.count, 0.U) -
              Mux(io.out.fire, 1.U, 0.U)
  bufValid := (bufValid | Mux(io.in.fire, UIntToOH(curXactHead), 0.U)) &
                         ~Mux(ren, UIntToOH(tail), 0.U)

  io.alloc.ready := !full
  io.in.ready := true.B
  io.out.valid := occupied
  io.out.bits := buffer.io.read.data

  buffer.io.write.en := io.in.fire
  buffer.io.write.addr := curXactHead
  buffer.io.write.data := io.in.bits.data
  buffer.io.read.en := ren
  buffer.io.read.addr := tail

  when (io.alloc.fire) {
    xactHeads(io.alloc.bits.id) := head
    head := incWrap(head, io.alloc.bits.count)
  }

  when (io.in.fire) {
    xactHeads(io.in.bits.id) := incWrap(curXactHead, 1.U)
  }

  when (io.out.fire) { occupied := false.B }

  when (ren) {
    occupied := true.B
    tail := incWrap(tail, 1.U)
  }
}

class PacketCollectionBuffer(bufWords: Int) extends Module {
  val io = IO(new StreamIO(NET_IF_WIDTH))

  val headerWords = ETH_HEAD_BYTES / NET_IF_BYTES
  val maxPackets = (bufWords - 1) / (headerWords + 1) + 1

  val queue = Module(new Queue(new StreamChannel(NET_IF_WIDTH), bufWords))
  val pktCount = TwoWayCounter(
    queue.io.enq.fire && queue.io.enq.bits.last,
    queue.io.deq.fire && queue.io.deq.bits.last,
    maxPackets)
  val canDequeue = pktCount > 0.U

  queue.io.enq <> io.in
  io.out.valid := queue.io.deq.valid && canDequeue
  io.out.bits := queue.io.deq.bits
  queue.io.deq.ready := io.out.ready && canDequeue
}
