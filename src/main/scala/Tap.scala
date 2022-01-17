package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.util.UIntIsOneOf
import scala.math.max
import testchipip._
import IceNetConsts._

class NetworkTap[T <: Data](
    selectFuncs: Seq[T => Bool],
    headerTyp: T = new EthernetHeader,
    headerBytes: Int = ETH_HEAD_BYTES,
    wordBytes: Int = NET_IF_WIDTH / 8) extends Module {

  val wordBits = wordBytes * 8
  val headerWords = headerBytes / wordBytes
  val n = selectFuncs.length

  val io = IO(new Bundle {
    val inflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val passthru = Decoupled(new StreamChannel(wordBits))
    val tapout = Vec(n, Decoupled(new StreamChannel(wordBits)))
  })

  assert(n > 0, "NetworkTap must have at least one output tap")

  val headerVec = Reg(Vec(headerWords, UInt(wordBits.W)))
  val header = headerVec.asTypeOf(headerTyp)

  val idxBits = log2Ceil(headerWords)
  val headerIdx = RegInit(0.U(idxBits.W))
  val headerLen = Reg(UInt(idxBits.W))
  val bodyLess = Reg(Bool())

  val (s_collect_header :: s_check_header ::
       s_output_header :: s_forward_body :: Nil) = Enum(4)
  val state = RegInit(s_collect_header)
  val route = Reg(Vec(n, Bool()))

  val selectedReady = MuxCase(io.passthru.ready,
    route.zip(io.tapout.map(_.ready)))

  io.inflow.ready := MuxLookup(state, false.B, Seq(
    s_collect_header -> true.B,
    s_forward_body -> selectedReady))

  val hasTapout = route.reduce(_ || _)

  io.passthru.valid := MuxLookup(Cat(state, hasTapout), false.B, Seq(
    Cat(s_output_header, false.B) -> true.B,
    Cat(s_forward_body,  false.B) -> io.inflow.valid))
  io.passthru.bits.data := MuxLookup(Cat(state, hasTapout), 0.U, Seq(
    Cat(s_output_header, false.B) -> headerVec(headerIdx),
    Cat(s_forward_body,  false.B) -> io.inflow.bits.data))
  io.passthru.bits.last := MuxLookup(Cat(state, hasTapout), false.B, Seq(
    Cat(s_output_header, false.B) -> (bodyLess && headerIdx === headerLen),
    Cat(s_forward_body,  false.B) -> io.inflow.bits.last))
  io.passthru.bits.keep := NET_FULL_KEEP

  io.tapout.zip(route).foreach { case (tapout, sel) =>
    tapout.valid := MuxLookup(Cat(state, sel), false.B, Seq(
      Cat(s_output_header, true.B) -> true.B,
      Cat(s_forward_body,  true.B) -> io.inflow.valid))
    tapout.bits.data := MuxLookup(Cat(state, sel), 0.U, Seq(
      Cat(s_output_header, true.B) -> headerVec(headerIdx),
      Cat(s_forward_body,  true.B) -> io.inflow.bits.data))
    tapout.bits.last := MuxLookup(Cat(state, sel), false.B, Seq(
      Cat(s_output_header, true.B) -> (bodyLess && headerIdx === headerLen),
      Cat(s_forward_body,  true.B) -> io.inflow.bits.last))
    tapout.bits.keep := NET_FULL_KEEP
  }

  when (state === s_collect_header && io.inflow.valid) {
    headerIdx := headerIdx + 1.U
    headerVec(headerIdx) := io.inflow.bits.data

    val headerLast = headerIdx === (headerWords-1).U

    when (io.inflow.bits.last || headerLast) {
      headerLen := headerIdx
      headerIdx := 0.U
      bodyLess := io.inflow.bits.last

      when (headerLast) {
        state := s_check_header
      } .otherwise {
        route.foreach(_ := false.B)
        state := s_output_header
      }
    }
  }

  when (state === s_check_header) {
    route := selectFuncs.map(_(header))
    state := s_output_header
  }

  val headerFire = state === s_output_header && selectedReady
  val bodyFire = state === s_forward_body && io.inflow.fire

  when (headerFire) {
    headerIdx := headerIdx + 1.U
    when (headerIdx === headerLen) {
      headerIdx := 0.U
      state := Mux(bodyLess, s_collect_header, s_forward_body)
    }
  }

  when (bodyFire && io.inflow.bits.last) { state := s_collect_header }
}

class NetworkTapTest extends UnitTest {
  val sendPayloads = Seq(
    Seq(0L, 0x800L << 48, 23L, 13L, 56L, 12L),
    Seq(0L, 0x800L << 48),
    Seq(0L, 0x801L << 48, 22, 16),
    Seq(0L))
  val sendLengths = sendPayloads.map(_.size)
  val sendData = sendPayloads.flatten.map(l => BigInt(l))

  val genIn = Module(new PacketGen(sendLengths, sendData))
  genIn.io.start := io.start

  val tapPayloads = sendPayloads.take(2)
  val tapData = tapPayloads.flatten.map(l => BigInt(l))
  val tapKeep = Seq.fill(tapData.size) { 0xff }
  val tapLast = tapPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkTap = Module(new PacketCheck(tapData, tapKeep, tapLast))

  val passPayloads = sendPayloads.drop(2)
  val passData = passPayloads.flatten.map(l => BigInt(l))
  val passKeep = Seq.fill(passData.size) { 0xff }
  val passLast = passPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkPass = Module(new PacketCheck(passData, passKeep, passLast))

  val tap = Module(new NetworkTap(
    Seq((header: EthernetHeader) => header.ethType === 0x800.U)))
  tap.io.inflow <> genIn.io.out
  checkTap.io.in <> tap.io.tapout(0)
  checkPass.io.in <> tap.io.passthru

  io.finished := checkTap.io.finished || checkPass.io.finished
}
