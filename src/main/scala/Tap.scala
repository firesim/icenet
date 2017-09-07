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
    headerTyp: T = new EthernetHeader,
    headerBytes: Int = ETH_HEAD_BYTES,
    wordBytes: Int = NET_IF_WIDTH / 8) extends Module {

  val wordBits = wordBytes * 8
  val headerWords = headerBytes / wordBytes

  val io = IO(new Bundle {
    val inflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val backflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val passthru = Decoupled(new StreamChannel(wordBits))
    val tapout = Decoupled(new StreamChannel(wordBits))
  })

  val headerVec = Reg(Vec(headerWords, UInt(wordBits.W)))
  val header = headerTyp.fromBits(headerVec.toBits)

  val idxBits = log2Ceil(headerWords)
  val headerIdx = RegInit(0.U(idxBits.W))
  val headerLen = Reg(UInt(idxBits.W))
  val bodyLess = Reg(Bool())

  val (s_inflow_header :: s_check_header ::
       s_passthru_header :: s_passthru_body ::
       s_tapout_body :: s_check_backflow ::
       s_backflow_header :: s_backflow_body :: Nil) = Enum(8)
  val state = RegInit(s_inflow_header)

  io.inflow.ready := MuxLookup(state, false.B, Seq(
    s_inflow_header -> true.B,
    s_passthru_body -> io.passthru.ready,
    s_tapout_body -> io.tapout.ready))

  io.backflow.ready := state === s_backflow_body && io.passthru.ready

  io.passthru.valid := MuxLookup(state, false.B, Seq(
    s_passthru_header -> true.B,
    s_passthru_body -> io.inflow.valid,
    s_backflow_header -> true.B,
    s_backflow_body -> io.backflow.valid))
  io.passthru.bits.data := MuxLookup(state, 0.U, Seq(
    s_passthru_header -> headerVec(headerIdx),
    s_passthru_body -> io.inflow.bits.data,
    s_backflow_header -> headerVec(headerIdx),
    s_backflow_body -> io.backflow.bits.data))
  io.passthru.bits.last := MuxLookup(state, false.B, Seq(
    s_passthru_header -> (bodyLess && headerIdx === headerLen),
    s_passthru_body -> io.inflow.bits.last,
    s_backflow_body -> io.backflow.bits.last))

  io.tapout.valid := state === s_tapout_body && io.inflow.valid
  io.tapout.bits := io.inflow.bits

  when (state === s_inflow_header && io.inflow.valid) {
    headerIdx := headerIdx + 1.U
    headerVec(headerIdx) := io.inflow.bits.data

    when (io.inflow.bits.last || headerIdx === (headerWords-1).U) {
      headerLen := headerIdx
      headerIdx := 0.U
      bodyLess := io.inflow.bits.last
      state := Mux(io.inflow.bits.last, s_passthru_header, s_check_header)
    }
  }

  when (state === s_check_header) {
    state := Mux(selectFunc(header), s_tapout_body, s_passthru_header)
  }

  when (io.passthru.fire()) {
    when (state.isOneOf(s_passthru_header, s_backflow_header)) {
      headerIdx := headerIdx + 1.U
      when (headerIdx === headerLen) {
        headerIdx := 0.U
        state := Mux(state === s_passthru_header,
          s_passthru_body, s_backflow_body)
      }
    }
    when (state.isOneOf(s_passthru_body, s_backflow_body)) {
      when (io.passthru.bits.last) { state := s_inflow_header }
    }
  }

  when (io.tapout.fire() && io.tapout.bits.last) {
    state := s_check_backflow
  }

  when (state === s_check_backflow) {
    state := Mux(io.backflow.valid, s_backflow_header, s_inflow_header)
  }
}

class NetworkTapTest extends UnitTest {
  val sendHeaders = Seq(
    EthernetHeader(1.U, 2.U, 0x800.U),
    EthernetHeader(1.U, 3.U, 0x800.U),
    EthernetHeader(1.U, 4.U, 0x801.U))
  val sendBodies = Seq(
    Seq(23.U, 13.U, 56.U, 12.U),
    Seq(),
    Seq(22.U, 16.U))
  val sendPayloads = sendHeaders.zip(sendBodies).map {
    case (header, body) => Vec(header.toWords() ++ body)
  }

  val sizes = sendPayloads.map(_.size)
  val maxSize = sizes.reduce(max(_, _))
  val nPhases = sizes.size

  val sendPhase = RegInit(0.U(log2Ceil(nPhases).W))
  val sendIdx = RegInit(0.U(log2Ceil(maxSize).W))
  val sendData = Vec(sendPayloads.map(pl => pl(sendIdx)))(sendPhase)
  val sendLast = Vec(sizes.map(sz => sendIdx === (sz-1).U))(sendPhase)
  val sending = RegInit(false.B)

  val tap = Module(new NetworkTap(
    (header: EthernetHeader) => header.ethType === 0x800.U))
  tap.io.inflow.valid := sending
  tap.io.inflow.bits.data := sendData
  tap.io.inflow.bits.last := sendLast

  val backQueue = Module(new Queue(new StreamChannel(NET_IF_WIDTH), maxSize))
  backQueue.io.enq <> tap.io.tapout
  tap.io.backflow.valid := backQueue.io.deq.valid
  tap.io.backflow.bits.data := backQueue.io.deq.bits.data + 1.U
  tap.io.backflow.bits.last := backQueue.io.deq.bits.last
  backQueue.io.deq.ready := tap.io.backflow.ready

  val recvBodies = sendBodies.head.map(_ + 1.U) +: sendBodies.tail
  val recvPayloads = sendHeaders.zip(recvBodies).map {
    case (header, body) => Vec(header.toWords() ++ body)
  }

  val recvPhase = RegInit(0.U(log2Ceil(nPhases).W))
  val recvIdx = RegInit(0.U(log2Ceil(maxSize).W))
  val recvData = Vec(recvPayloads.map(pl => pl(recvIdx)))(recvPhase)
  val recvLast = Vec(sizes.map(sz => recvIdx === (sz-1).U))(recvPhase)
  val receiving = RegInit(false.B)

  tap.io.passthru.ready := receiving

  val started = RegInit(false.B)

  when (!started && io.start) {
    started := true.B
    sending := true.B
    receiving := true.B
  }

  when (tap.io.inflow.fire()) {
    sendIdx := sendIdx + 1.U
    when (sendLast) {
      sendIdx := 0.U
      sendPhase := sendPhase + 1.U
      when (sendPhase === (nPhases - 1).U) { sending := false.B }
    }
  }

  when (tap.io.passthru.fire()) {
    recvIdx := recvIdx + 1.U
    when (recvLast) {
      recvIdx := 0.U
      recvPhase := recvPhase + 1.U
      when (recvPhase === (nPhases - 1).U) { receiving := false.B }
    }
  }

  io.finished := started && !(sending || receiving)

  assert(!tap.io.passthru.valid ||
         (tap.io.passthru.bits.data === recvData &&
          tap.io.passthru.bits.last === recvLast),
      "NetworkTapTest: received data or last incorrect")
}
