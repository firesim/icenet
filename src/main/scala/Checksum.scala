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

class IPChecksumBackend(dataBits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(dataBits)))
    val out = Decoupled(UInt(16.W))
  })

  val s_accum :: s_fold :: s_squash :: s_out :: Nil = Enum(4)
  val state = RegInit(s_accum)

  val n = dataBits / 16
  val sums = RegInit(VecInit(Seq.fill(n) { 0.U(32.W) }))
  val enables = Seq.tabulate(n) { i => io.in.bits.keep(2*i+1, 2*i).andR }
  val words = Seq.tabulate(n) { i => io.in.bits.data(16*(i+1)-1, 16*i) }
  val foldIdx = Reg(UInt(log2Ceil(n).W))

  when (io.in.fire()) {
    (sums, enables, words).zipped.foreach {
      case (sum, en, word) => when (en) { sum := sum + word }
    }
    when (io.in.bits.last) { foldIdx := 1.U; state := s_fold }
  }

  when (state === s_fold) {
    sums(0) := sums(0) + sums(foldIdx)
    foldIdx := foldIdx + 1.U
    when (foldIdx === (n-1).U) { state := s_squash }
  }

  when (state === s_squash) {
    val sum = sums.head
    when (sum(31, 16) =/= 0.U) {
      sum := sum(31, 16) + sum(15, 0)
    } .otherwise { state := s_out }
  }

  when (io.out.fire()) {
    sums.foreach(sum => sum := 0.U)
    state := s_accum
  }

  io.in.ready := state === s_accum
  io.out.valid := state === s_out
  io.out.bits := ~sums.head(15, 0)
}

class IPChecksumFrontend(maxBytes: Int)
    (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode("ip-checksum", IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val dataBits = tl.params.dataBits
    val addrBits = tl.params.addressBits
    val beatBytes = dataBits / 8
    val maxBeats = maxBytes / beatBytes
    val byteAddrBits = log2Ceil(beatBytes)

    val io = IO(new Bundle {
      val request = Flipped(Decoupled(UInt(64.W)))
      val stream = Decoupled(new StreamChannel(dataBits))
    })

    val addr = Reg(UInt(addrBits.W))
    val beatAddr = addr >> byteAddrBits.U
    val byteAddr = addr(byteAddrBits - 1, 0)
    val len = Reg(UInt(15.W))
    val beatsLeft = Reg(UInt(log2Ceil(maxBeats+1).W))
    val bytesEnd = byteAddr + len
    val mask = Cat(((beatBytes-1) to 0 by -1).map(
      i => i.U >= byteAddr && i.U < bytesEnd))
    val partial = Reg(Bool())

    val reqSize = MuxCase(byteAddrBits.U,
      (log2Ceil(maxBytes) until byteAddrBits by -1).map(lgSize =>
        (addr(lgSize-1, 0) === 0.U && (len >> lgSize.U) =/= 0.U) -> lgSize.U))

    val s_idle :: s_acquire :: s_grant :: Nil = Enum(3)
    val state = RegInit(s_idle)

    io.request.ready := state === s_idle

    io.stream.valid := (state === s_grant) && tl.d.valid
    io.stream.bits.data := tl.d.bits.data
    io.stream.bits.keep := mask
    io.stream.bits.last := len <= beatBytes.U && !partial

    tl.a.valid := state === s_acquire
    tl.a.bits := edge.Get(
      fromSource = 0.U,
      toAddress = Cat(addr >> byteAddrBits.U, 0.U(byteAddrBits.W)),
      lgSize = reqSize)._2
    tl.d.ready := (state === s_grant) && io.stream.ready

    when (io.request.fire()) {
      addr := io.request.bits(47, 0)
      len := io.request.bits(62, 48)
      partial := io.request.bits(63)
      state := s_acquire
    }

    when (tl.a.fire()) {
      assert(tl.a.bits.address(0) === 0.U, "Request must be aligned to 2 bytes")
      beatsLeft := 1.U << (reqSize - byteAddrBits.U)
      state := s_grant
    }

    when (tl.d.fire()) {
      val bytesSent = beatBytes.U - byteAddr
      val nextLen = Mux(bytesSent <= len, len - bytesSent, 0.U)

      beatsLeft := beatsLeft - 1.U
      addr := Cat(beatAddr + 1.U, 0.U(byteAddrBits.W))
      len := nextLen

      state := MuxCase(s_grant, Seq(
        (nextLen === 0.U) -> s_idle,
        (beatsLeft === 1.U) -> s_acquire))
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
    val backend = Module(new IPChecksumBackend(frontend.module.dataBits))
    frontend.module.io.request <> io.request
    backend.io.in <> Queue(frontend.module.io.stream, 2)
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

  val expected = calcChecksum(
    testWords.slice(1, 31) ++ testWords.slice(34, 63))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    val s_start :: s_request :: s_response :: s_finished :: Nil = Enum(4)
    val state = RegInit(s_start)

    val addrs = VecInit(Seq(2, 68).map(_.U(48.W)))
    val lens  = VecInit(Seq(60, 58).map(_.U(15.W)))
    val idx = RegInit(0.U(1.W))

    val checksumIO = checksum.module.io
    checksumIO.request.valid := state === s_request
    checksumIO.request.bits := Cat(idx =/= 1.U, lens(idx), addrs(idx))
    checksumIO.response.ready := state === s_response
    io.finished := state === s_finished

    when (state === s_start && io.start) { state := s_request }
    when (checksumIO.request.fire()) {
      idx := idx + 1.U
      when (idx === 1.U) { state := s_response }
    }
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
