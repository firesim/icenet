package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import scala.util.Random
import testchipip.{StreamIO, StreamChannel}
import IceNetConsts._

class BufferBRAM[T <: Data](n: Int, typ: T) extends Module {
  val addrBits = log2Ceil(n)
  val io = IO(new Bundle {
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

class NetworkPacketBuffer[T <: Data](
    nPackets: Int,
    maxBytes: Int = ETH_MAX_BYTES,
    headerBytes: Int = ETH_HEAD_BYTES,
    headerType: T = new EthernetHeader(64),
    wordBytes: Int = 8) extends Module {

  val maxWords = maxBytes / wordBytes
  val headerWords = headerBytes / wordBytes
  val wordBits = wordBytes * 8

  val idxBits = log2Ceil(maxWords + 1)
  val phaseBits = log2Ceil(nPackets)

  val io = IO(new Bundle {
    val stream = new StreamIO(wordBytes * 8)
    val header = Valid(headerType)
    val length = Output(UInt(idxBits.W))
    val count = Output(UInt(log2Ceil(nPackets+1).W))
  })

  assert(!io.stream.in.valid || io.stream.in.bits.keep.andR, "NetworkPacketBuffer does not handle missing data")

  val buffers = Seq.fill(nPackets) { Module(new BufferBRAM(maxWords, Bits(wordBits.W))) }
  val headers = Seq.fill(nPackets) { Reg(Vec(headerWords, Bits(wordBits.W))) }
  val bufLengths = Seq.fill(nPackets) { RegInit(0.U(idxBits.W)) }
  val bufValid = VecInit(bufLengths.map(len => len > 0.U))

  val inIdx = RegInit(0.U(idxBits.W))
  val inDrop = RegInit(false.B)
  val outIdx = RegInit(0.U(idxBits.W))

  val inPhase = RegInit(0.U(phaseBits.W))
  val outPhase = RegInit(0.U(phaseBits.W))

  //AJG: Problematic area
  //There is REg of vectors that is being accessed
  //So headers(idx) gets a specific amount of headerWords
  //val outHeader = VecInit(headers.map(header => headerType.asTypeOf(Cat(header.reverse))))
  //                                                                This is not HW? This just makes a .U element
  //                                                                the goal of this is to construct a headerType of object
  val outHeader = VecInit(headers.map(header => Cat(header.reverse).asTypeOf(headerType)))
  val outLast = VecInit(bufLengths.map(len => outIdx === (len - 1.U)))
  val outValidReg = RegInit(false.B)

  val ren = (io.stream.out.ready || !outValidReg) && bufValid(outPhase)
  val wen = WireInit(false.B)
  val hwen = wen && inIdx < headerWords.U

  val outPhaseReg = RegEnable(outPhase, ren)
  val outDataReg = VecInit(buffers.map(buffer => buffer.io.read.data))(outPhaseReg)
  val outLastReg = RegEnable(outLast(outPhase), ren)
  val outIdxReg = RegEnable(outIdx, ren)

  io.stream.out.valid := outValidReg
  io.stream.out.bits.data := outDataReg
  io.stream.out.bits.last := outLastReg
  io.stream.out.bits.keep := DontCare
  io.stream.in.ready := true.B
  io.header.valid := bufValid(outPhase)
  io.header.bits := outHeader(outPhase)
  io.length := RegEnable(VecInit(bufLengths)(outPhase), ren) - outIdxReg
  io.count := RegEnable(PopCount(bufValid), ren)

  when (io.stream.out.fire()) { outValidReg := false.B }
  when (ren) { outValidReg := true.B }

  def wrapInc(x: UInt) = Mux(x === (nPackets - 1).U, 0.U, x + 1.U)

  val bufLenSet = WireInit(false.B)
  val bufLenClear = WireInit(false.B)

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
      when (hwen) { header(inIdx) := io.stream.in.bits.data }
    }
    when (outPhase === i.U) {
      when (bufLenClear) { bufLen := 0.U }
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
    wen := !startDropping && !inDrop
    when (inIdx =/= maxWords.U) { inIdx := inIdx + 1.U }

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
  val networkConfig = new IceNetConfig(NET_IF_WIDTH=64)
  val buffer = Module(new NetworkPacketBuffer(nPackets=2, maxBytes=32, headerBytes=8, headerType=UInt(64.W), wordBytes=(networkConfig.NET_IF_WIDTH/8)))
  val rnd = new Random
  val nPackets = 64
  val phaseBits = log2Ceil(nPackets)
  val packetLengths = VecInit(Seq.fill(nPackets) { (2 + rnd.nextInt(6)).U(3.W) })

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
    when (buffer.io.stream.out.bits.last) { outIdx := 0.U }
  }

  assert(!buffer.io.stream.out.valid || buffer.io.stream.out.bits.data === outIdx,
    "NetworkPacketBufferTest: got output data out of order")
  assert(!buffer.io.header.valid || buffer.io.header.bits === (1L << 32).U,
    "NetworkPacketBufferTest: unexpected header")

  io.finished := started && !sending && !buffer.io.stream.out.valid
}

class ReservationBufferAlloc(nXacts: Int, nWords: Int) extends Bundle {
  private val xactIdBits = log2Ceil(nXacts)
  private val countBits = log2Ceil(nWords + 1)

  val id = UInt(xactIdBits.W)
  val count = UInt(countBits.W)

  override def cloneType =
    new ReservationBufferAlloc(nXacts, nWords).asInstanceOf[this.type]
}

class ReservationBufferData(nXacts: Int, ifWidth: Int) extends Bundle {
  private val xactIdBits = log2Ceil(nXacts)

  val id = UInt(xactIdBits.W)
  val data = new StreamChannel(ifWidth)

  override def cloneType =
    new ReservationBufferData(nXacts, ifWidth).asInstanceOf[this.type]
}

class ReservationBuffer(nXacts: Int, nWords: Int, ifWidth: Int) extends Module {
  private val xactIdBits = log2Ceil(nXacts)
  private val countBits = log2Ceil(nWords + 1)

  require(nXacts <= nWords)

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(new ReservationBufferAlloc(nXacts, nWords)))
    val in = Flipped(Decoupled(new ReservationBufferData(nXacts, ifWidth)))
    val out = Decoupled(new StreamChannel(ifWidth))
  })

  def incWrap(cur: UInt, inc: UInt): UInt = {
    val unwrapped = cur +& inc
    Mux(unwrapped >= nWords.U, unwrapped - nWords.U, unwrapped)
  }

  val buffer = Module(new BufferBRAM(nWords, new StreamChannel(ifWidth)))
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
              Mux(io.alloc.fire(), io.alloc.bits.count, 0.U) -
              Mux(io.out.fire(), 1.U, 0.U)
  bufValid := (bufValid | Mux(io.in.fire(), UIntToOH(curXactHead), 0.U)) &
                         ~Mux(ren, UIntToOH(tail), 0.U)

  io.alloc.ready := !full
  io.in.ready := true.B
  io.out.valid := occupied
  io.out.bits := buffer.io.read.data

  buffer.io.write.en := io.in.fire()
  buffer.io.write.addr := curXactHead
  buffer.io.write.data := io.in.bits.data
  buffer.io.read.en := ren
  buffer.io.read.addr := tail

  when (io.alloc.fire()) {
    xactHeads(io.alloc.bits.id) := head
    head := incWrap(head, io.alloc.bits.count)
  }

  when (io.in.fire()) {
    xactHeads(io.in.bits.id) := incWrap(curXactHead, 1.U)
  }

  when (io.out.fire()) { occupied := false.B }

  when (ren) {
    occupied := true.B
    tail := incWrap(tail, 1.U)
  }
}
