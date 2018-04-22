package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink.TLROM
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.unittest.{UnitTest, UnitTestIO}
import freechips.rocketchip.tilelink._
import testchipip.{StreamChannel, TLHelper}
import scala.util.Random

class IPChecksumBackend extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(16)))
    val out = Decoupled(UInt(16.W))
  })

  val s_accum :: s_fold :: s_out :: Nil = Enum(3)
  val state = RegInit(s_accum)

  val sum = RegInit(0.U(32.W))

  when (io.in.fire()) {
    sum := sum + io.in.bits.data
    when (io.in.bits.last) { state := s_fold }
  }

  when (state === s_fold) {
    when (sum(31, 16) =/= 0.U) {
      sum := sum(31, 16) + sum(15, 0)
    } .otherwise { state := s_out }
  }

  when (io.out.fire()) {
    sum := 0.U
    state := s_accum
  }

  io.in.ready := state === s_accum
  io.out.valid := state === s_out
  io.out.bits := ~sum(15, 0)
}

class IPChecksumFrontend(maxBytes: Int)
    (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode("ip-checksum", IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val request = Flipped(Decoupled(UInt(64.W)))
      val stream = Decoupled(new StreamChannel(16))
    })

    val (tl, edge) = node.out(0)

    val dataBits = tl.params.dataBits
    val addrBits = tl.params.addressBits
    val beatBytes = dataBits / 8
    val maxBeats = maxBytes / beatBytes
    val byteOffsetBits = log2Ceil(beatBytes)
    val beatOffsetBits = log2Ceil(maxBeats)

    val data = Reg(UInt(dataBits.W))
    val addr = Reg(UInt(addrBits.W))
    val byteOffset = addr(byteOffsetBits - 1, 0)
    val beatOffset = addr(beatOffsetBits + byteOffsetBits - 1, byteOffsetBits)
    val len = Reg(UInt(16.W))
    val beatsLeft = Reg(UInt((beatOffsetBits+1).W))

    val reqSize = MuxCase(byteOffsetBits.U,
      (log2Ceil(maxBytes) until byteOffsetBits by -1).map(lgSize =>
        (addr(lgSize-1, 0) === 0.U && (len >> lgSize.U) =/= 0.U) -> lgSize.U))

    val s_idle :: s_acquire :: s_grant :: s_stream :: Nil = Enum(4)
    val state = RegInit(s_idle)

    io.request.ready := state === s_idle

    io.stream.valid := state === s_stream
    io.stream.bits.data := data(15, 0)
    io.stream.bits.keep := DontCare
    io.stream.bits.last := len === 2.U

    tl.a.valid := state === s_acquire
    tl.a.bits := edge.Get(
      fromSource = 0.U,
      toAddress = Cat(addr >> byteOffsetBits.U, 0.U(byteOffsetBits.W)),
      lgSize = reqSize)._2
    tl.d.ready := state === s_grant

    when (io.request.fire()) {
      addr := io.request.bits(47, 0)
      len := io.request.bits(63, 48)
      state := s_acquire
    }

    when (tl.a.fire()) {
      assert(tl.a.bits.address(0) === 0.U, "Request must be aligned to 2 bytes")
      beatsLeft := 1.U << (reqSize - byteOffsetBits.U)
      state := s_grant
    }

    when (tl.d.fire()) {
      data := tl.d.bits.data >> Cat(byteOffset, 0.U(3.W))
      beatsLeft := beatsLeft - 1.U
      state := s_stream
    }

    when (io.stream.fire()) {
      data := data >> 16.U
      addr := addr + 2.U
      len  := len - 2.U
      val lastWord = byteOffset === (beatBytes-2).U
      val lastBeat = beatsLeft === 0.U
      state := MuxCase(s_stream, Seq(
        io.stream.bits.last -> s_idle,
        (lastWord && lastBeat) -> s_acquire,
        (lastWord && !lastBeat) -> s_grant))
    }
  }
}

class IPChecksumIO extends Bundle {
  val request = Decoupled(UInt(64.W))
  val response = Flipped(Decoupled(UInt(16.W)))
}

class IPChecksum(maxBytes: Int)(implicit p: Parameters) extends LazyModule {
  val frontend = LazyModule(new IPChecksumFrontend(maxBytes))

  val node = frontend.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(Flipped(new IPChecksumIO))
    val backend = Module(new IPChecksumBackend)
    frontend.module.io.request <> io.request
    backend.io.in <> frontend.module.io.stream
    io.response <> backend.io.out
  }
}

class IPChecksumTest(implicit p: Parameters) extends LazyModule {
  val beatBytes = 8
  val maxBytes = 64

  val testData = Seq.fill(128) { Random.nextInt(128).toByte }
  val rom = LazyModule(new TLROM(
    0x0, testData.size, testData,
    beatBytes = beatBytes))
  val checksum = LazyModule(new IPChecksum(maxBytes))

  rom.node := TLFragmenter(beatBytes, maxBytes) := TLBuffer() := checksum.node

  val testWords = (0 until testData.size by 2).map(
    i => ((testData(i).toInt & 0xff) | (testData(i+1).toInt << 8)))

  def calcChecksum(words: Seq[Int]) = {
    var sum = words.map(_ & 0xffff).foldLeft(0)(_ + _)
    while ((sum >> 16) != 0)
      sum = (sum >> 16) + (sum & 0xffff)
    ~sum & 0xffff
  }

  val expected = calcChecksum(testWords.drop(1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    val s_start :: s_request :: s_response :: s_finished :: Nil = Enum(4)
    val state = RegInit(s_start)

    val addr = 2.U(48.W)
    val len = 126.U(16.W)

    val checksumIO = checksum.module.io
    checksumIO.request.valid := state === s_request
    checksumIO.request.bits := Cat(len, addr)
    checksumIO.response.ready := state === s_response
    io.finished := state === s_finished

    when (state === s_start && io.start) { state := s_request }
    when (checksumIO.request.fire()) { state := s_response }
    when (checksumIO.response.fire()) { state := s_finished }

    assert(!checksumIO.response.valid ||
            checksumIO.response.bits === expected.U,
        "Checksum response not correct")
  }
}

class IPChecksumTestWrapper(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new IPChecksumTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}
