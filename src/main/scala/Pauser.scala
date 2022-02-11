package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{HellaPeekingArbiter}
import freechips.rocketchip.unittest.UnitTest
import testchipip.{StreamIO, StreamChannel}
import IceNetConsts._

object PauseConsts {
  val MAC_ETHTYPE = 0x8808
  val PAUSE_CTRL = 0x0001
  val BT_PER_QUANTA = 512
  val CYCLES_PER_QUANTA = BT_PER_QUANTA / NET_IF_WIDTH
  val MULTICAST_MACADDR = 0x010000C28001L
}
import PauseConsts._

trait NetworkEndianHelpers {
  def ntohs(x: UInt) = Cat(x(7, 0), x(15, 8))
  def htons(x: UInt) = ntohs(x)
  def ntohl(x: UInt) = Cat(ntohs(x(15, 0)), ntohs(x(31, 16)))
  def htonl(x: UInt) = ntohl(x)
}

class PauserSettings extends Bundle {
  val threshold = UInt(16.W)
  val quanta    = UInt(16.W)
  val refresh   = UInt(16.W)
}

object PauseDropCheck extends NetworkEndianHelpers {
  def apply(header: EthernetHeader, ch: StreamChannel, update: Bool): Bool = {
    val first = RegInit(true.B)
    val isPause = ntohs(header.ethType) === MAC_ETHTYPE.U &&
                  ntohs(ch.data(15, 0)) === PAUSE_CTRL.U && first
    val isPauseReg = RegInit(false.B)

    when (update && first)  { first := false.B; isPauseReg := isPause }
    when (update && ch.last) { first := true.B; isPauseReg := false.B }

    isPause || isPauseReg
  }
}

/**
 * Flow control unit using Ethernet pause frames
 * See https://en.wikipedia.org/wiki/Ethernet_flow_control#Pause_frame
 * @creditInit Size of each buffer being tracked
 * @nBuckets Number of buffers being tracked
 */
class Pauser(creditInit: Int, nBuckets: Int) extends Module
    with NetworkEndianHelpers {
  val timerBits = 16 + log2Ceil(CYCLES_PER_QUANTA)
  val creditBits = log2Ceil(creditInit + 1)

  val io = IO(new Bundle {
    val ext = new StreamIO(NET_IF_WIDTH)
    val int = Flipped(new StreamIO(NET_IF_WIDTH))
    val in_free = Input(Vec(nBuckets, UInt(8.W)))
    val macAddr = Input(UInt(48.W))
    val settings = Input(new PauserSettings)
  })

  val credits = RegInit(VecInit(Seq.fill(nBuckets)(creditInit.U(16.W))))
  val outPauseTimer = RegInit(0.U(timerBits.W))
  val outPaused = outPauseTimer > 0.U

  io.int.in <> io.ext.in

  val headerBeats = ETH_HEAD_BYTES / NET_IF_BYTES
  val inHeaderVec = Reg(Vec(headerBeats, UInt(NET_IF_WIDTH.W)))
  val inHeader = inHeaderVec.asTypeOf(new EthernetHeader)

  val s_head :: s_mac :: s_tail :: Nil = Enum(3)
  val state = RegInit(s_head)

  val inHeadIdx = RegInit(0.U(log2Ceil(headerBeats).W))

  when (outPaused) { outPauseTimer := outPauseTimer - 1.U }

  when (io.int.in.fire) {
    val data = io.int.in.bits.data

    switch (state) {
      is (s_head) {
        inHeaderVec(inHeadIdx) := data
        inHeadIdx := inHeadIdx + 1.U
        when (inHeadIdx === (headerBeats-1).U) { state := s_mac }
      }
      is (s_mac) {
        val isMac = ntohs(inHeader.ethType) === MAC_ETHTYPE.U
        val isPause = ntohs(data(15, 0)) === PAUSE_CTRL.U
        val quanta = ntohs(data(31, 16))
        val cycles = quanta << log2Ceil(CYCLES_PER_QUANTA).U
        when (isMac && isPause) { outPauseTimer := cycles }
        state := s_tail
      }
    }

    when (io.int.in.bits.last) {
      state := s_head
      inHeadIdx := 0.U
    }
  }

  for (i <- 0 until nBuckets) {
    credits(i) := credits(i) - io.int.in.fire + io.in_free(i)
  }

  val arb = Module(new PacketArbiter(2))

  val outHeader = EthernetHeader(
    MULTICAST_MACADDR.U,
    io.macAddr,
    htons(MAC_ETHTYPE.U(16.W)))
  val outVec = VecInit(outHeader.toWords() :+ Cat(
    htons(io.settings.quanta), htons(PAUSE_CTRL.U(16.W))))
  val sendPause = RegInit(false.B)
  val (outIdx, outDone) = Counter(arb.io.in(0).fire, outVec.size)

  arb.io.in(0).valid := sendPause
  arb.io.in(0).bits.data := outVec(outIdx)
  arb.io.in(0).bits.keep := NET_FULL_KEEP
  arb.io.in(0).bits.last := outIdx === (outVec.size-1).U

  val inPauseTimer = RegInit(0.U(16.W))
  val inPaused = inPauseTimer > 0.U

  val creditsLow = credits.map(_ < io.settings.threshold).reduce(_ || _)

  when (outDone) {
    inPauseTimer := io.settings.refresh
    sendPause := false.B
  }
  when (inPaused) { inPauseTimer := inPauseTimer - 1.U }
  when (creditsLow && !sendPause && !inPaused) {
    sendPause := true.B
  }

  val outInProgress = RegInit(false.B)
  val canForward = !outPaused || outInProgress

  arb.io.in(1).valid := canForward && io.int.out.valid
  arb.io.in(1).bits := io.int.out.bits
  io.int.out.ready := canForward && arb.io.in(1).ready

  when (arb.io.in(1).fire) {
    when (!outInProgress) { outInProgress := true.B }
    when (arb.io.in(1).bits.last) { outInProgress := false.B }
  }

  io.ext.out <> arb.io.out
}

class PauserComplex(nFlits: Int) extends Module {
  val creditBits = log2Ceil(nFlits + 1)
  val io = IO(new Bundle {
    val ext = new StreamIO(NET_IF_WIDTH)
    val int = Flipped(new StreamIO(NET_IF_WIDTH))
    val macAddr = Input(UInt(48.W))
    val settings = Input(new PauserSettings)
  })

  val pauser = Module(new Pauser(nFlits, 1))
  val buffer = Module(new NetworkPacketBuffer(
    nFlits, dropChecks = Seq(PauseDropCheck(_, _, _)), dropless = true))

  io.ext <> pauser.io.ext
  pauser.io.macAddr := io.macAddr
  pauser.io.settings := io.settings

  pauser.io.int.out <> io.int.out
  buffer.io.stream.in <> pauser.io.int.in
  pauser.io.in_free(0) := buffer.io.free
  io.int.in <> buffer.io.stream.out
}

class PauserTest extends UnitTest {
  val nFlits = 400
  val latency = 128
  val packetWords = ETH_STANDARD_MAX_BYTES / NET_IF_BYTES
  val threshold = latency + packetWords
  val pauseQuanta = threshold / CYCLES_PER_QUANTA
  val pauseRefresh = packetWords

  val lcomplex = Module(new PauserComplex(nFlits))
  val rcomplex = Module(new PauserComplex(nFlits))

  rcomplex.io.ext.flipConnect(NetDelay(lcomplex.io.ext, latency))
  rcomplex.io.int.out <> rcomplex.io.int.in

  val lmacaddr = (0x2L << 40).U(48.W)
  val rmacaddr = (0x3L << 40).U(48.W)

  lcomplex.io.macAddr := lmacaddr
  rcomplex.io.macAddr := rmacaddr
  lcomplex.io.settings.threshold := threshold.U
  rcomplex.io.settings.threshold := threshold.U
  lcomplex.io.settings.quanta := pauseQuanta.U
  rcomplex.io.settings.quanta := pauseQuanta.U
  lcomplex.io.settings.refresh := pauseRefresh.U
  rcomplex.io.settings.refresh := pauseRefresh.U

  val ethHeader = EthernetHeader(rmacaddr, lmacaddr, 0x0000.U)
  val pktData = VecInit(
    ethHeader.toWords() ++ Seq.tabulate(130)(_.U(NET_IF_WIDTH.W)))
  val nPackets = 8

  val started = RegInit(false.B)

  val sending = RegInit(false.B)
  val (sendIdx, sendPktDone) = Counter(lcomplex.io.int.out.fire, pktData.size)
  val (sendPhase, sendDone) = Counter(sendPktDone, nPackets)

  val receiving = RegInit(false.B)
  val (recvIdx, recvPktDone) = Counter(lcomplex.io.int.in.fire, pktData.size)
  val (recvPhase, recvDone) = Counter(recvPktDone, nPackets)

  lcomplex.io.int.out.valid := sending
  lcomplex.io.int.out.bits.data := pktData(sendIdx)
  lcomplex.io.int.out.bits.keep := NET_FULL_KEEP
  lcomplex.io.int.out.bits.last := sendIdx === (pktData.size-1).U
  lcomplex.io.int.in.ready := receiving

  when (!started && io.start) {
    started := true.B
    sending := true.B
    receiving := true.B
  }
  when (sendDone) { sending := false.B }
  when (recvDone) { receiving := false.B }

  val recv = lcomplex.io.int.in
  assert(!recv.valid ||
    (recv.bits.data === pktData(recvIdx) &&
     recv.bits.keep === NET_FULL_KEEP &&
     recv.bits.last === (recvIdx === (pktData.size-1).U)),
    "PauserTest: received incorrect data, keep, or last")

  io.finished := started && !sending && !receiving
}
