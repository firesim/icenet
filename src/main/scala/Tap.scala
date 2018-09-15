package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.util.UIntIsOneOf
import scala.math.max
import testchipip._
import IceNetConsts._

class NetworkTap[T <: Data](
    selectFunc: T => Bool,
    headerType: T = new EthernetHeader(64),
    headerBytes: Int = ETH_HEAD_BYTES,
    wordBytes: Int) extends Module {

  val wordBits = wordBytes * 8
  val headerWords = headerBytes / wordBytes

  val io = IO(new Bundle {
    val inflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val passthru = Decoupled(new StreamChannel(wordBits))
    val tapout = Decoupled(new StreamChannel(wordBits))
  })

  val headerVec = Reg(Vec(headerWords, UInt(wordBits.W)))
  //val header = headerType.fromBits(headerVec.toBits)
  val header = headerVec.asUInt().asTypeOf(headerType)

  val idxBits = log2Ceil(headerWords)
  val headerIdx = RegInit(0.U(idxBits.W))
  val headerLen = Reg(UInt(idxBits.W))
  val bodyLess = Reg(Bool())

  val (s_inflow_header :: s_check_header ::
       s_passthru_header :: s_passthru_body ::
       s_tapout_header :: s_tapout_body :: Nil) = Enum(6)
  val state = RegInit(s_inflow_header)

  io.inflow.ready := MuxLookup(state, false.B, Seq(
    s_inflow_header -> true.B,
    s_passthru_body -> io.passthru.ready,
    s_tapout_body -> io.tapout.ready))

  io.passthru.valid := MuxLookup(state, false.B, Seq(
    s_passthru_header -> true.B,
    s_passthru_body -> io.inflow.valid))
  io.passthru.bits.data := MuxLookup(state, 0.U, Seq(
    s_passthru_header -> headerVec(headerIdx),
    s_passthru_body -> io.inflow.bits.data))
  io.passthru.bits.last := MuxLookup(state, false.B, Seq(
    s_passthru_header -> (bodyLess && headerIdx === headerLen),
    s_passthru_body -> io.inflow.bits.last))

  // AJG: TODO: Added this to compile. Is this OK? 
  io.passthru.bits.keep := DontCare
  io.tapout.bits.keep := DontCare

  io.tapout.valid := MuxLookup(state, false.B, Seq(
    s_tapout_header -> true.B,
    s_tapout_body -> io.inflow.valid))
  io.tapout.bits.data := MuxLookup(state, 0.U, Seq(
    s_tapout_header -> headerVec(headerIdx),
    s_tapout_body -> io.inflow.bits.data))
  io.tapout.bits.last := MuxLookup(state, false.B, Seq(
    s_tapout_header -> (bodyLess && headerIdx === headerLen),
    s_tapout_body -> io.inflow.bits.last))

  when (state === s_inflow_header && io.inflow.valid) {
    headerIdx := headerIdx + 1.U
    headerVec(headerIdx) := io.inflow.bits.data

    val headerLast = headerIdx === (headerWords-1).U

    when (io.inflow.bits.last || headerLast) {
      headerLen := headerIdx
      headerIdx := 0.U
      bodyLess := io.inflow.bits.last
      state := Mux(headerLast, s_check_header, s_passthru_header)
    }
  }

  when (state === s_check_header) {
    state := Mux(selectFunc(header), s_tapout_header, s_passthru_header)
  }

  val header_fire = state === s_passthru_header && io.passthru.ready ||
                    state === s_tapout_header   && io.tapout.ready
  val body_fire = state.isOneOf(s_passthru_body, s_tapout_body) && io.inflow.fire()

  when (header_fire) {
    headerIdx := headerIdx + 1.U
    when (headerIdx === headerLen) {
      headerIdx := 0.U
      state := Mux(bodyLess, s_inflow_header,
        Mux(state === s_passthru_header, s_passthru_body, s_tapout_body))
    }
  }

  when (body_fire && io.inflow.bits.last) { state := s_inflow_header }
}

class NetworkTapTest extends UnitTest {
  val sendPayloads = Seq(
    Seq(0L, 0x800L << 48, 23L, 13L, 56L, 12L),
    Seq(0L, 0x800L << 48),
    Seq(0L, 0x801L << 48, 22, 16),
    Seq(0L))
  val sendLengths = sendPayloads.map(_.size)
  val sendData = sendPayloads.flatten.map(l => BigInt(l))
  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = 64) 

  val genIn = Module(new PacketGen(sendLengths, sendData, netConfig))
  genIn.io.start := io.start

  val tapPayloads = sendPayloads.take(2)
  val tapData = tapPayloads.flatten.map(l => BigInt(l))
  val tapKeep = Seq.fill(tapData.size) { 0xff }
  val tapLast = tapPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkTap = Module(new PacketCheck(tapData, tapKeep, tapLast, netConfig))

  val passPayloads = sendPayloads.drop(2)
  val passData = passPayloads.flatten.map(l => BigInt(l))
  val passKeep = Seq.fill(passData.size) { 0xff }
  val passLast = passPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkPass = Module(new PacketCheck(passData, passKeep, passLast, netConfig))

  val tap = Module(new NetworkTap((header: EthernetHeader) => header.ethType === 0x800.U, headerType = new EthernetHeader(netConfig.NET_IF_WIDTH_BITS), wordBytes = netConfig.NET_IF_WIDTH_BYTES))
  tap.io.inflow <> genIn.io.out
  checkTap.io.in <> tap.io.tapout
  checkPass.io.in <> tap.io.passthru
  checkTap.io.in.bits.keep := netConfig.NET_FULL_KEEP 
  checkPass.io.in.bits.keep := netConfig.NET_FULL_KEEP 

  io.finished := checkTap.io.finished || checkPass.io.finished
}
