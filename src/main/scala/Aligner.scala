package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

/**
 * This functional block is used to take an input stream and shift it's input by a certain amount in 
 * bytes. This shifting includes it's "keep" values and making sure that the last flag corresponds to the
 * end of the input stream correctly.
 *
 * @param netConfig configuration parameters for network
 */
class StreamShifter(netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle{
    val stream = new StreamIO(netConfig.NET_IF_WIDTH_BITS) // input stream of data
    val addrOffsetBytes = Input(UInt(log2Ceil(netConfig.NET_IF_WIDTH_BYTES).W)) // amount to bytes to shift by
  })

  // Note: these masks add extra bits since << adds and doesn't cut off
  val addrOffsetBits = io.addrOffsetBytes * 8.U
  val upperMaskBits = ~0.U(netConfig.NET_IF_WIDTH_BITS.W) << addrOffsetBits
  val lowerMaskBits = ~upperMaskBits
  val upperMaskKeep = ~0.U(netConfig.NET_IF_WIDTH_BYTES.W) << io.addrOffsetBytes
  val lowerMaskKeep = ~upperMaskKeep

  // data to be sent when ready
  val send_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W))
  val send_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  // data held for next round of sending
  val backup_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W))
  val backup_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))

  // rotated data that is used for moving data to right positions
  val rotated_data = (io.stream.in.bits.data << addrOffsetBits) | (io.stream.in.bits.data >> (netConfig.NET_IF_WIDTH_BITS.U - addrOffsetBits))
  val rotated_keep = (io.stream.in.bits.keep << io.addrOffsetBytes) | (io.stream.in.bits.keep >> (netConfig.NET_IF_WIDTH_BYTES.U - io.addrOffsetBytes))

  // keep track of how many bytes are in the shifter 
  val leftToSendBytes = RegInit(0.U((2*log2Ceil(netConfig.NET_IF_WIDTH_BYTES)).W))
  val outputSendBytes = PopCount(send_keep)
  val doneWithOutput = (leftToSendBytes <= outputSendBytes)

  val inValidDelayOne = RegNext(io.stream.in.valid)
  val processingLast = RegInit(false.B) // indicates when a last flag is seen and the packet is finishing

  io.stream.in.ready := Mux(doneWithOutput, io.stream.out.ready, Mux(processingLast, false.B, io.stream.out.ready))

  io.stream.out.valid := inValidDelayOne || processingLast || (leftToSendBytes > 0.U)
  io.stream.out.bits.data := send_data
  io.stream.out.bits.keep := send_keep
  io.stream.out.bits.last := doneWithOutput // send last signal when there are no more bytes to send

  when (io.stream.in.fire() && io.stream.out.fire()) {
    // when something enters and something else leaves
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 
    leftToSendBytes := Mux(outputSendBytes > leftToSendBytes + PopCount(io.stream.in.bits.keep), 0.U, leftToSendBytes + PopCount(io.stream.in.bits.keep) - outputSendBytes)
    processingLast := io.stream.in.bits.last
  }
  .elsewhen (io.stream.in.fire()) {
    // when something comes in and nothing goes out yet
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 
    leftToSendBytes := leftToSendBytes + PopCount(io.stream.in.bits.keep)
    processingLast := io.stream.in.bits.last
  }
  .elsewhen (io.stream.out.fire()) {
    // when something is output from the pipeline but nothing is put in thus make the next values 0
    send_data := (backup_data & lowerMaskBits)
    backup_data := 0.U
    send_keep := (backup_keep & lowerMaskKeep)
    backup_keep := 0.U
    leftToSendBytes := Mux(outputSendBytes > leftToSendBytes, 0.U, leftToSendBytes - outputSendBytes)
    processingLast := Mux(io.stream.out.bits.last, false.B, processingLast)
  }
}

/**
 * This functional block is used to align the data read in from the reservation buffer. Specifically, the data
 * read in from memory is aligned by 8B. However, two issues occur. 1. The packet data may start at a different
 * memory address that is not at a multiple of 8B. 2. The packet may have invalid data within it. Thus, this
 * aligner must shift this read in data and remove all invalid data to send out only the valid data from the 
 * packet (and not any other gibberish data).
 *
 * @param netConfig configuration parameters for network
 */
class Aligner(netConfig: IceNetConfig) extends Module {
  val io = IO(new StreamIO(netConfig.NET_IF_WIDTH_BITS))

  val data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W))
  val keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  val last = RegInit(false.B)
  val nbytes = RegInit(0.U(log2Ceil(netConfig.NET_IF_WIDTH_BYTES + 1).W))

  // AJG: should this be andR (was originally orR)
  assert(!io.in.valid || io.in.bits.keep.orR, "Aligner cannot handle an empty flit")

  val rshift = PriorityEncoder(io.in.bits.keep)
  val full_keep = ((io.in.bits.keep >> rshift) << nbytes) | keep // hold all keep for multiple data packets

  val rshift_bit = Cat(rshift, 0.U(3.W)) // =rshift*8
  val nbits = Cat(nbytes, 0.U(3.W)) // =nbytes*8
  val bitmask = FillInterleaved(8, keep)
  val full_data = ((io.in.bits.data >> rshift_bit) << nbits) | (data & bitmask) // hold all data for multiple data packets
  val full_nbytes = PopCount(full_keep) // amount of bytes stored
  val fwd_last = io.in.bits.last && (full_keep >> netConfig.NET_IF_WIDTH_BYTES.U) === 0.U // if input packet is last but when shifted fits in the packet to be send

  io.out.valid := (last && nbytes > 0.U) ||
                  (io.in.valid && (fwd_last || full_nbytes >= netConfig.NET_IF_WIDTH_BYTES.U))
  io.out.bits.data := Mux(last, data, full_data(netConfig.NET_IF_WIDTH_BITS-1, 0))
  io.out.bits.keep := Mux(last, keep, full_keep(netConfig.NET_IF_WIDTH_BYTES-1, 0))
  io.out.bits.last := last || fwd_last

  io.in.ready := full_nbytes < netConfig.NET_IF_WIDTH_BYTES.U ||
                 (io.out.ready && !last)

  when (io.in.fire() && io.out.fire()) {
    data := full_data >> netConfig.NET_IF_WIDTH_BITS.U // can get rid of NET_IF_WIDTH worth of data since done
    keep := full_keep >> netConfig.NET_IF_WIDTH_BYTES.U // can get rid of NET_IF_WIDTH worth of data since done
    last := io.in.bits.last && !fwd_last
    nbytes := Mux(fwd_last, 0.U, full_nbytes - netConfig.NET_IF_WIDTH_BYTES.U)
  }
  .elsewhen (io.in.fire()) {
    data := full_data
    keep := full_keep
    last := io.in.bits.last
    nbytes := full_nbytes
  }
  .elsewhen (io.out.fire()) {
    data := 0.U
    keep := 0.U
    last := false.B
    nbytes := 0.U
  }
}

/**
 * Unit test for Aligner class
 *
 * @param testWidth size of the network interface for test
 */
class AlignerTest(testWidth: Int = 64) extends UnitTest {
  // send two packets worth of information
  val inData = VecInit(
    "h0011223344556677".U,
    "h8899AABBCCDDEEFF".U,
    "h0123456789ABCDEF".U,
    "hFEDCBA9876543210".U)
  val inKeep = VecInit(
    "b11111100".U,
    "b01111000".U,
    "b00001111".U,
    "b11110000".U)
  val inLast = VecInit(false.B, false.B, true.B, true.B)

  // output should have all valid bytes until the end
  val outData = VecInit(
    "hBBCC001122334455".U,
    "h000089ABCDEF99AA".U,
    "h00000000FEDCBA98".U)
  val outKeep = VecInit(
    "b11111111".U,
    "b00111111".U,
    "b00001111".U)
  val outLast = VecInit(false.B, true.B, true.B) // still should be two packets 

  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val aligner = Module(new Aligner(new IceNetConfig(NET_IF_WIDTH_BITS = testWidth)))

  val (inIdx, inDone) = Counter(aligner.io.in.fire(), inData.size)
  val (outIdx, outDone) = Counter(aligner.io.out.fire(), outData.size)

  aligner.io.in.valid := sending
  aligner.io.in.bits.data := inData(inIdx)
  aligner.io.in.bits.keep := inKeep(inIdx)
  aligner.io.in.bits.last := inLast(inIdx)
  aligner.io.out.ready := receiving

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B 
  }
  when (inDone) { sending := false.B }
  when (outDone) { receiving := false.B }

  io.finished := started && !sending && !receiving

  def compareData(a: UInt, b: UInt, keep: UInt) = {
    val bitmask = FillInterleaved(8, keep)
    (a & bitmask) === (b & bitmask)
  }

  assert(!aligner.io.out.valid ||
    (compareData(
      aligner.io.out.bits.data,
      outData(outIdx),
      aligner.io.out.bits.keep) &&
     aligner.io.out.bits.keep === outKeep(outIdx) &&
     aligner.io.out.bits.last === outLast(outIdx)),
   "AlignerTest: output does not match expected")
}

/**
 * Unit test for StreamShifter class
 *
 * @param testWidth size of the network interface for test
 */
class StreamShifterTest(testWidth: Int = 64) extends UnitTest {
  // send multiple packets worth of information
  val inData = VecInit( "h0011223344556677".U, "h8899AABBCCDDEEFF".U, "h0123456789ABCDEF".U,
                        "h7766554433221100".U,
                        "hFEDCBA9876543210".U,
                        "hDEADBEEF00112233".U,
                        "h1234567890123456".U )
  val inKeep = VecInit( "b11111111".U, "b11111111".U, "b00001111".U,
                        "b00000011".U,
                        "b00001111".U,
                        "b00111111".U,
                        "b11111111".U )
  val inLast = VecInit( false.B, false.B, true.B,
                        true.B,
                        true.B,
                        true.B,
                        true.B )

  // output should have just shifted by 4B on everything
  val outData = VecInit( "h4455667700000000".U, "hCCDDEEFF00112233".U, "h89ABCDEF8899AABB".U,
                         "h3322110000000000".U,
                         "h7654321000000000".U,
                         "h0011223300000000".U, "h00000000DEADBEEF".U,
                         "h9012345600000000".U, "h0000000012345678".U )
  val outKeep = VecInit( "b11110000".U, "b11111111".U, "b11111111".U,
                         "b00110000".U,
                         "b11110000".U,
                         "b11110000".U, "b00000011".U,
                         "b11110000".U, "b00001111".U )
  val outLast = VecInit( false.B, false.B, true.B,
                         true.B,
                         true.B,
                         false.B, true.B,
                         false.B, true.B )

  val shiftByteAmount = 4.U

  val started = RegInit(false.B)
  val sending = RegInit(false.B)
  val receiving = RegInit(false.B)

  val streamShifter = Module(new StreamShifter(new IceNetConfig(NET_IF_WIDTH_BITS = testWidth)))

  val (inIdx, inDone) = Counter(streamShifter.io.stream.in.fire(), inData.size)
  val (outIdx, outDone) = Counter(streamShifter.io.stream.out.fire(), outData.size)

  streamShifter.io.stream.in.valid := sending
  streamShifter.io.stream.in.bits.data := inData(inIdx)
  streamShifter.io.stream.in.bits.keep := inKeep(inIdx)
  streamShifter.io.stream.in.bits.last := inLast(inIdx)
  streamShifter.io.stream.out.ready := receiving
  streamShifter.io.addrOffsetBytes := shiftByteAmount // pass in shift amount

  when (io.start && !started) {
    started := true.B
    sending := true.B
    receiving := true.B
  }
  // when (started && !outDone) { receiving := !receiving } // Test when the output is not ready immediately
  when (inDone) { sending := false.B }
  when (outDone) { receiving := false.B }

  io.finished := started && !sending && !receiving

  def compareData(a: UInt, b: UInt, keep: UInt) = {
    val bitmask = FillInterleaved(8, keep)
    (a & bitmask) === (b & bitmask)
  }

  assert(!streamShifter.io.stream.out.fire() ||
         (compareData( streamShifter.io.stream.out.bits.data, outData(outIdx), streamShifter.io.stream.out.bits.keep) &&
         streamShifter.io.stream.out.bits.keep === outKeep(outIdx) &&
         streamShifter.io.stream.out.bits.last === outLast(outIdx)),
         "StreamShifterTest: output does not match expected")
}
