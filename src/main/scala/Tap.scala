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
 * @param selectFunc function to choose which tap to select
 * @param headerType class representing the header
 * @param headerBypes size of the header class in bytes
 * @param wordBytes size of flit in bytes
 */
class NetworkTap[T <: Data](
    selectFuncs: Seq[T => Bool],
    headerTyp: T = new EthernetHeader,
    headerBytes: Int = ETH_HEAD_BYTES,
    wordBytes: Int) extends Module {

  val wordBits = wordBytes * 8
  val headerWords = if(wordBytes > headerBytes) 1 else headerBytes/wordBytes
  val n = selectFuncs.length

  val io = IO(new Bundle {
    val inflow = Flipped(Decoupled(new StreamChannel(wordBits)))
    val passthru = Decoupled(new StreamChannel(wordBits))
    val tapout = Vec(n, Decoupled(new StreamChannel(wordBits)))
  })

  assert(n > 0, "NetworkTap must have at least one output tap")

  val headerVec = Reg(Vec(headerWords, UInt(wordBits.W)))
  val header = headerVec.asUInt().asTypeOf(headerTyp) 

  val idxBits = if(headerWords < 2) 1 else log2Ceil(headerWords)
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
  io.passthru.bits.keep := MuxLookup(Cat(state, hasTapout), 0.U, Seq(
    Cat(s_output_header, false.B) -> ~0.U(wordBytes.W),
    Cat(s_forward_body,  false.B) -> io.inflow.bits.keep))

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
    tapout.bits.keep := MuxLookup(Cat(state, sel), 0.U, Seq(
      Cat(s_output_header, true.B) -> ~0.U(wordBytes.W),
      Cat(s_forward_body,  true.B) -> io.inflow.bits.keep))
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

  // use passed in function to determine if it is a tapout or passthru
  when (state === s_check_header) {
    route := selectFuncs.map(_(header))
    state := s_output_header
  }

  val headerFire = state === s_output_header && selectedReady
  val bodyFire = state === s_forward_body && io.inflow.fire()

  when (headerFire) {
    headerIdx := headerIdx + 1.U
    when (headerIdx === headerLen) {
      headerIdx := 0.U
      state := Mux(bodyLess, s_collect_header, s_forward_body)
    }
  }

  when (bodyFire && io.inflow.bits.last) { state := s_collect_header }
}

/**
 * Unit test for the NetworkTap function
 */
class NetworkTapTest(netIfWidthBits: Int = 64) extends UnitTest {

  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = netIfWidthBits)

  // send a payload using a ehternet header
  val sendPayloads = if (testWidth > 64){
    // make sure ethernet header is in the LSB of the flit
    Seq( Seq( BigInt(0x800) << (ETH_HEAD_BYTES*8 - ETH_TYPE_BITS), BigInt(23), BigInt(13), BigInt(56), BigInt(12) ),
         Seq( BigInt(0x800) << (ETH_HEAD_BYTES*8 - ETH_TYPE_BITS) ),
         Seq( BigInt(0x801) << (ETH_HEAD_BYTES*8 - ETH_TYPE_BITS), BigInt(22), BigInt(16) ),
         Seq( BigInt(0) ) )
  }
  else {
    // note: here the ethernet header is split between the 1st and 2nd flits
    Seq( Seq( BigInt(0L), BigInt(0x800L << ETH_MAC_BITS), BigInt(23L), BigInt(13L), BigInt(56L), BigInt(12L) ),
         Seq( BigInt(0L), BigInt(0x800L << ETH_MAC_BITS) ),
         Seq( BigInt(0L), BigInt(0x801L << ETH_MAC_BITS), BigInt(22), BigInt(16) ),
         Seq( BigInt(0L)) )
  }     

  val sendLengths = sendPayloads.map(_.size)
  val sendData = sendPayloads.flatten
  val sendKeep = Seq.fill(sendPayloads.size){ (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 }

  // create packets based off the send* section
  val genIn = Module(new PacketGen(sendLengths, sendData, sendKeep, netConfig))
  genIn.io.start := io.start

  // create tapout data to check with
  val tapPayloads = sendPayloads.take(2)
  val tapData = tapPayloads.flatten
  // create a BigInt mask corresponding to a full mask 
  val tapKeep = Seq.fill(tapData.size) { (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 }
  val tapLast = tapPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkTap = Module(new PacketCheck(tapData, tapKeep, tapLast, netConfig))

  // create passthru data to check with
  val passPayloads = sendPayloads.drop(2)
  val passData = passPayloads.flatten
  // create a BigInt mask corresponding to a full mask 
  val passKeep = Seq.fill(passData.size) { (BigInt(1) << (netConfig.NET_IF_WIDTH_BYTES)) - 1 }
  val passLast = passPayloads.flatMap {
    pl => (Seq.fill(pl.size-1) { false } ++ Seq(true))
  }
  val checkPass = Module(new PacketCheck(passData, passKeep, passLast, netConfig))

  val tap = Module(new NetworkTap(
    Seq((header: EthernetHeader) => header.ethType === 0x800.U),
    wordBytes = netConfig.NET_IF_WIDTH_BYTES))
  tap.io.inflow <> genIn.io.out
  checkTap.io.in <> tap.io.tapout(0)
  checkPass.io.in <> tap.io.passthru

  io.finished := checkTap.io.finished || checkPass.io.finished
}
