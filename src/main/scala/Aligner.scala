package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

/**
 * This functional block is used to take an input stream and shift it's input by a certain amount in 
 * bytes. This shifting includes it's "keep" values and making sure that the last value is kept.
 *
 * @param netConfig the config parameters for the NIC
 * @param addrOffsetBytes the amount of offset that you have to shift left
 */
class AlignToAddr(netConfig: IceNetConfig, addrOffsetBytes: UInt) extends Module {
  // Setup input and output stream channels
  val io = IO(new StreamIO(netConfig.NET_IF_WIDTH_BITS))

  // Note: These masks add extra bits since << adds and doesn't cut off
  val addrOffsetBits = addrOffsetBytes * 8.U
  val upperMaskBits = ~0.U(netConfig.NET_IF_WIDTH_BITS.W) << addrOffsetBits
  val lowerMaskBits = ~upperMaskBits
  val upperMaskKeep = ~0.U(netConfig.NET_IF_WIDTH_BYTES.W) << addrOffsetBytes
  val lowerMaskKeep = ~upperMaskKeep

  // Data to be sent when ready
  val send_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W)) //data to be sent when ready
  val send_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  // Data held for next round of sending
  val backup_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W)) //data storage for next round
  val backup_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  val validDelayOne = RegNext(io.in.valid)

  // Rotated data that is used for moving data to right positions
  val rotated_data = (io.in.bits.data << addrOffsetBits) | (io.in.bits.data >> (netConfig.NET_IF_WIDTH_BITS.U - addrOffsetBits))
  val rotated_keep = (io.in.bits.keep << addrOffsetBytes) | (io.in.bits.keep >> (netConfig.NET_IF_WIDTH_BYTES.U - addrOffsetBytes))

  val leftToSendBytes = RegInit(0.U((2*log2Ceil(netConfig.NET_IF_WIDTH_BYTES)).W))
  val outputSendBytes = PopCount(send_keep)
  val doneWithOutput = (leftToSendBytes <= outputSendBytes)

  // TODO: Cleanup block to remove this logic
  val send_last = RegInit(false.B)
  val backup_last = RegInit(false.B)
  when( io.in.fire() ){
    when ( PriorityEncoder(~io.in.bits.keep) +& addrOffsetBytes <= netConfig.NET_IF_WIDTH_BYTES.U ){
      send_last := io.in.bits.last | backup_last
      backup_last := false.B
    }
    .otherwise{
      send_last := backup_last
      backup_last := io.in.bits.last
    }
  }
  .otherwise{
    // When IO does not fire make sure to continue moving the mark down
    send_last := backup_last
    backup_last := false.B
  }

  // Check if the last flag has been seen (cleared when there are no more bytes to send for the current shift)
  val hasSeenLast = RegInit(false.B)
  hasSeenLast := Mux(io.in.valid && io.in.bits.last, true.B, Mux(doneWithOutput, false.B, hasSeenLast))

  io.in.ready := doneWithOutput || !hasSeenLast // Ready to read in new data when no more bytes to read and you haven't seen last signal

  io.out.bits.data := send_data
  io.out.bits.keep := send_keep
  // TODO: Clean up the io.out.valid signal
  io.out.valid := send_last || validDelayOne // There should always be an output after 1 delay or if there is a send_last signal
  io.out.bits.last := doneWithOutput // Send last signal when there are no more bytes to send

  when (io.in.fire() && io.out.fire()) {
    // When something enters and something else leaves
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 
    leftToSendBytes := Mux(outputSendBytes > leftToSendBytes + PopCount(io.in.bits.keep), 0.U, leftToSendBytes + PopCount(io.in.bits.keep) - outputSendBytes)
  }
  .elsewhen (io.in.fire()) {
    // When something comes in and nothing goes out yet
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 
    leftToSendBytes := leftToSendBytes + PopCount(io.in.bits.keep)
  }
  .elsewhen (io.out.fire()) {
    // When something is output from the pipeline but nothing is put in thus make the next values 0
    send_data := (backup_data & lowerMaskBits)
    backup_data := 0.U
    send_keep := (backup_keep & lowerMaskKeep)
    backup_keep := 0.U
    leftToSendBytes := Mux(outputSendBytes > leftToSendBytes, 0.U, leftToSendBytes - outputSendBytes)
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

  // AJG: Should this be andR (was originally orR)
  assert(!io.in.valid || io.in.bits.keep.orR, "Aligner cannot handle an empty flit")

  val rshift = PriorityEncoder(io.in.bits.keep)
  val full_keep = ((io.in.bits.keep >> rshift) << nbytes) | keep

  val rshift_bit = Cat(rshift, 0.U(3.W))
  val nbits = Cat(nbytes, 0.U(3.W))
  val bitmask = FillInterleaved(8, keep)
  val full_data = ((io.in.bits.data >> rshift_bit) << nbits) | (data & bitmask)
  val full_nbytes = PopCount(full_keep)
  val fwd_last = io.in.bits.last && (full_keep >> netConfig.NET_IF_WIDTH_BYTES.U) === 0.U

  io.out.valid := (last && nbytes > 0.U) ||
                  (io.in.valid && (fwd_last || full_nbytes >= netConfig.NET_IF_WIDTH_BYTES.U))
  io.out.bits.data := Mux(last, data, full_data(netConfig.NET_IF_WIDTH_BITS-1, 0))
  io.out.bits.keep := Mux(last, keep, full_keep(netConfig.NET_IF_WIDTH_BYTES-1, 0))
  io.out.bits.last := last || fwd_last

  io.in.ready := full_nbytes < netConfig.NET_IF_WIDTH_BYTES.U ||
                 (io.out.ready && !last)

  when (io.in.fire() && io.out.fire()) {
    data := full_data >> netConfig.NET_IF_WIDTH_BITS.U
    keep := full_keep >> netConfig.NET_IF_WIDTH_BYTES.U
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
  // Send two packets worth of information
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

  // Output should have all valid bytes until the end
  val outData = VecInit(
    "hBBCC001122334455".U,
    "h000089ABCDEF99AA".U,
    "h00000000FEDCBA98".U)
  val outKeep = VecInit(
    "b11111111".U,
    "b00111111".U,
    "b00001111".U)
  val outLast = VecInit(false.B, true.B, true.B) // Still should be two packets 

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
 * Unit test for AlignDataToAddr class
 *
 * @param testWidth size of the network interface for test
 */
class AlignDataToAddrTest(testWidth: Int = 64) extends UnitTest {
  // Send multiple packets worth of information
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

  // Output should have just shifted by 4B on everything
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

  val aligner = Module(new AlignToAddr(new IceNetConfig(NET_IF_WIDTH_BITS = testWidth), shiftByteAmount))

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
         (compareData( aligner.io.out.bits.data, outData(outIdx), aligner.io.out.bits.keep) &&
         aligner.io.out.bits.keep === outKeep(outIdx) &&
         aligner.io.out.bits.last === outLast(outIdx)),
         "AlignDataToAddrTest: output does not match expected")
}
