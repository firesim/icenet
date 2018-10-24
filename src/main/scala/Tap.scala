package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.util.UIntIsOneOf
import scala.math.max
import testchipip._
import IceNetConsts._

/**
 * Create a network tap that filters out particular packets to another hardware module.
 *
 * @param selectFunc function to select the ...
 * @param headerType class representing the header
 * @param headerBypes size of the header class in bytes
 * @param wordBytes size of flit in bytes
 */
class NetworkTap[T <: Data](
    selectFunc: T => Bool,
    headerType: T = new EthernetHeader,
    headerBytes: Int = ETH_HEAD_BYTES,
    wordBytes: Int) extends Module {

  val wordBits = wordBytes * 8
  val headerWords = if(wordBytes > headerBytes) 1 else headerBytes/wordBytes

  val io = IO(new Bundle {
    val inflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val passthru = Decoupled(new StreamChannel(wordBits))
    val tapout = Decoupled(new StreamChannel(wordBits))
  })

  val headerDataVec = Reg(Vec(headerWords, UInt(wordBits.W)))
  val headerKeepVec = Reg(Vec(headerWords, UInt(wordBytes.W)))

  // note: if wordBytes > headerBytes then the header will be taken from the most significant bits
  val header = if (wordBytes > headerBytes){
    (headerDataVec.asUInt() >> (wordBits - headerBytes*8)).asTypeOf(headerType)
  }
  else {
    headerDataVec.asUInt().asTypeOf(headerType)
  }

  val idxBits = if(headerWords < 2) 1 else log2Ceil(headerWords)
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

  // connect passthru signals to header signals
  io.passthru.valid := MuxLookup(state, false.B, Seq(
    s_passthru_header -> true.B,
    s_passthru_body -> io.inflow.valid))
  io.passthru.bits.data := MuxLookup(state, 0.U, Seq(
    s_passthru_header -> headerDataVec(headerIdx),
    s_passthru_body -> io.inflow.bits.data))
  io.passthru.bits.last := MuxLookup(state, false.B, Seq(
    s_passthru_header -> (bodyLess && headerIdx === headerLen),
    s_passthru_body -> io.inflow.bits.last))
  io.passthru.bits.keep := MuxLookup(state, 0.U, Seq(
    s_passthru_header -> headerKeepVec(headerIdx),
    s_passthru_body -> io.inflow.bits.keep))

  // connect tapout signals to header signals (identical to passthru)
  io.tapout.valid := MuxLookup(state, false.B, Seq(
    s_tapout_header -> true.B,
    s_tapout_body -> io.inflow.valid))
  io.tapout.bits.data := MuxLookup(state, 0.U, Seq(
    s_tapout_header -> headerDataVec(headerIdx),
    s_tapout_body -> io.inflow.bits.data))
  io.tapout.bits.last := MuxLookup(state, false.B, Seq(
    s_tapout_header -> (bodyLess && headerIdx === headerLen),
    s_tapout_body -> io.inflow.bits.last))
  io.tapout.bits.keep := MuxLookup(state, 0.U, Seq(
    s_tapout_header -> headerKeepVec(headerIdx),
    s_tapout_body -> io.inflow.bits.keep))

  when (state === s_inflow_header && io.inflow.valid) {
    headerIdx := headerIdx + 1.U
    headerDataVec(headerIdx) := io.inflow.bits.data
    headerKeepVec(headerIdx) := io.inflow.bits.keep

    val headerLast = headerIdx === (headerWords-1).U

    when (io.inflow.bits.last || headerLast) {
      headerLen := headerIdx
      headerIdx := 0.U
      bodyLess := io.inflow.bits.last
      state := Mux(headerLast, s_check_header, s_passthru_header)
    }
  }

  // when in check_header state, use passed in function to determine if it is a tapout or passthru
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

/**
 * Unit test for NetworkTap
 */
class NetworkTapTest(testWidth: Int = 64) extends UnitTest {
  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = testWidth) 

  // send a payload using a ethernet header
  val sendPayloads = if (testWidth > 64){
    // make sure ethernet header is in the MSB of the flit
    Seq( Seq( BigInt( 0x800 ) << ((ETH_HEAD_BYTES*8 - ETH_TYPE_BITS) + (testWidth - ETH_HEAD_BYTES*8)), BigInt(23), BigInt(13), BigInt(56), BigInt(12) ),
         Seq( BigInt( 0x800 ) << ((ETH_HEAD_BYTES*8 - ETH_TYPE_BITS) + (testWidth - ETH_HEAD_BYTES*8)) ),
         Seq( BigInt( 0x801 ) << ((ETH_HEAD_BYTES*8 - ETH_TYPE_BITS) + (testWidth - ETH_HEAD_BYTES*8)), BigInt(22), BigInt(16) ),
         Seq( BigInt( 0 ) ) )
  }
  else {
    // note: here the ethernet header is split between the 1st and 2nd flits
    Seq( Seq(BigInt(0L), BigInt(0x800L << ETH_MAC_BITS), BigInt(23L), BigInt(13L), BigInt(56L), BigInt(12L)),
         Seq(BigInt(0L), BigInt(0x800L << ETH_MAC_BITS)),
         Seq(BigInt(0L), BigInt(0x801L << ETH_MAC_BITS), BigInt(22), BigInt(16)),
         Seq(BigInt(0L)) )
  }
  val sendLengths = sendPayloads.map(_.size)
  val sendData = sendPayloads.flatten
  val sendKeep = Seq.fill(sendPayloads.size) { (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 }

  // create packets based off the send* section
  val genIn = Module(new PacketGen(sendLengths, sendData, sendKeep, netConfig))

  // create tapout data to check with
  val tapPayloads = sendPayloads.take(2)
  val tapData = tapPayloads.flatten
  val tapKeep = Seq.fill(tapData.size) { (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 } // create a BigInt mask corresponding to a full mask 
  val tapLast = tapPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkTap = Module(new PacketCheck(tapData, tapKeep, tapLast, netConfig))

  // create passthru data to check with
  val passPayloads = sendPayloads.drop(2)
  val passData = passPayloads.flatten
  val passKeep = Seq.fill(passData.size) { (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 } // create a BigInt mask corresponding to a full mask 
  val passLast = passPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkPass = Module(new PacketCheck(passData, passKeep, passLast, netConfig))

  // DUT
  val tap = Module(new NetworkTap((header: EthernetHeader) => header.ethType === 0x800.U, headerType = new EthernetHeader, wordBytes = netConfig.NET_IF_WIDTH_BYTES))

  genIn.io.start := io.start // create the packets
  tap.io.inflow <> genIn.io.out // pass into DUT

  // connect checkers for the passthru and tapout
  checkTap.io.in <> tap.io.tapout
  checkPass.io.in <> tap.io.passthru

  io.finished := checkTap.io.finished || checkPass.io.finished
}
