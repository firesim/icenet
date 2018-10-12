package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import testchipip.StreamIO
import IceNetConsts._

/**
 * This functional block is used to take an input stream and shift it's input by a input amount in 
 * bytes.
 *
 * @param netConfig the config parameters for the NIC
 * @param addrOffsetBytes the amount of offset that you have to shift left
 */
class AlignToAddr(netConfig: IceNetConfig, addrOffsetBytes: UInt) extends Module {
  // Setup input and output stream channels
  val io = IO(new StreamIO(netConfig.NET_IF_WIDTH_BITS))

  // Note: These masks add extra bits since << adds and doesn't cut off
  val addrOffsetBits = addrOffsetBytes * 8.U
  val upperMaskBits = ~0.U(netConfig.NET_IF_WIDTH_BITS.W) << addrOffsetBits // keep the upper bits
  val lowerMaskBits = ~upperMaskBits // keep the lower bits
  val upperMaskKeep = ~0.U(netConfig.NET_IF_WIDTH_BYTES.W) << addrOffsetBytes
  val lowerMaskKeep = ~upperMaskKeep // keep the upper bits

  // Data to be sent when ready
  val send_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W)) //data to be sent when ready
  val send_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  val backup_data = RegInit(0.U(netConfig.NET_IF_WIDTH_BITS.W)) //data storage for next round
  val backup_keep = RegInit(0.U(netConfig.NET_IF_WIDTH_BYTES.W))
  val lastDelayOne = RegNext(io.in.bits.last)
  val lastDelayTwo = RegNext(lastDelayOne)
  val validDelayOne = RegNext(io.in.valid)

  val iovalid = io.in.valid
  val iodata = io.in.bits.data
  val iokeep = io.in.bits.keep
  val iolast = io.in.bits.last

  val rotated_data = (io.in.bits.data << addrOffsetBits) | (io.in.bits.data >> (netConfig.NET_IF_WIDTH_BITS.U - addrOffsetBits))
  val rotated_keep = (io.in.bits.keep << addrOffsetBytes) | (io.in.bits.keep >> (netConfig.NET_IF_WIDTH_BYTES.U - addrOffsetBytes))

  printf( "--- DUT DEBUG ---\n" )
  printf( p"DUT: back_data(0x${Hexadecimal(backup_data)}) back_keep(0x${Hexadecimal(backup_keep)})\n" )
  printf( p"DUT: send_data(0x${Hexadecimal(send_data)}) send_keep(0x${Hexadecimal(send_keep)})\n" )
  printf( p"DUT: rota_data(0x${Hexadecimal(rotated_data)}) rotated_keep(0x${Hexadecimal(rotated_keep)})\n" )
  printf( p"DUT: validDelayOne($validDelayOne)\n" )// lastDelayOne($lastDelayOne) lastDelayTwo($lastDelayTwo)\n" )
  when(iovalid){
    printf( p"DUT: in: data(0x${Hexadecimal(iodata)}) keep(0x${Hexadecimal(iokeep)}) last(${iolast})\n" )
  }

  // if the bottom of the back_keep is 0 then this is the last if lastDelayOne is also high
  // Depending if the offset prevents the last thing from showing
  //val send_last = Mux(addrOffsetBytes === 0.U, lastDelayOne, 
  //                Mux((backup_keep & lowerMaskKeep) === 0.U, lastDelayOne, lastDelayTwo))

  val overflow = io.in.valid && ((PriorityEncoder(~io.in.bits.keep) +& addrOffsetBytes) > netConfig.NET_IF_WIDTH_BYTES.U)
  when(overflow){
    printf( "DUT: There is overflow\n" )
  }
  .otherwise{
    printf( "DUT: There is no overflow\n" )
  }
  val markLast = RegInit(false.B)
  val markLast2 = RegInit(false.B)
  val addition = PriorityEncoder(~io.in.bits.keep) +& addrOffsetBytes
  val against = netConfig.NET_IF_WIDTH_BYTES.U 
  printf( p"DUT: markLast($markLast) markLast2($markLast2)\n" )
  when( io.in.fire() ){
    when ( PriorityEncoder(~io.in.bits.keep) +& addrOffsetBytes <= netConfig.NET_IF_WIDTH_BYTES.U ){
      printf(p"DUT: Less: $addition <= $against\n")
      markLast := io.in.bits.last | markLast2
      markLast2 := false.B
    }
    .otherwise{
      printf(p"DUT: Greater: $addition > $against\n")
      markLast := markLast2
      markLast2 := io.in.bits.last
    }
  }
  .otherwise{
    // When IO does not fire make sure to continue moving the mark down
    printf("DUT: Move markLast2 => markLast\n" )
    markLast := markLast2
    markLast2 := false.B
  }

  val send_last = markLast

  io.out.bits.data := send_data
  io.out.bits.keep := send_keep
  
  //io.out.bits.last := send_last 

  // TODO: WHEN SHOULD YOU READ IN DATA?
  val isCurrentLast = io.in.valid && io.in.bits.last
  val leftToSendBytes = RegInit(0.U((2*log2Ceil(netConfig.NET_IF_WIDTH_BYTES)).W))
  val outputSendBytes = PopCount(send_keep)
  val doneWithOutput = (leftToSendBytes <= outputSendBytes)
  printf( p"DUT: leftToSend($leftToSendBytes) outputSend($outputSendBytes)\n" )
  when(io.out.fire() && io.in.fire()){
    when(outputSendBytes > leftToSendBytes + PopCount(io.in.bits.keep)){
      printf("DUT: LTS <= 0\n")
      leftToSendBytes := 0.U
    }
    .otherwise{
      printf("DUT: add and sub to LTS\n")
      leftToSendBytes := leftToSendBytes + PopCount(io.in.bits.keep) - outputSendBytes
    }
  }
  .elsewhen(io.in.fire()){
    printf( "DUT: addition to leftToSend\n" )
    leftToSendBytes := leftToSendBytes + PopCount(io.in.bits.keep)
  }
  .elsewhen(io.out.fire()){
    when(outputSendBytes > leftToSendBytes){
      printf("zero LTS\n")
      leftToSendBytes := 0.U
    }
    .otherwise{
      printf("sub from LTS\n")
      leftToSendBytes := leftToSendBytes - outputSendBytes
    }
  }

  val hasSeenLast = RegInit(false.B)
  when( isCurrentLast ){
    hasSeenLast := true.B
  }
  .otherwise{
    when(doneWithOutput){
      hasSeenLast := false.B
    }
    .otherwise{
      hasSeenLast := hasSeenLast
    }
  }
  io.out.valid := send_last || validDelayOne
  //io.out.valid := doneWithOutput && hasSeenLast
  io.in.ready := (leftToSendBytes <= outputSendBytes) || !hasSeenLast//isCurrentLast

  io.out.bits.last := (leftToSendBytes <= outputSendBytes)
  //io.in.ready := (io.out.ready)

  val outvalid = io.out.valid
  val outdata = io.out.bits.data
  val outkeep = io.out.bits.keep
  val outlast = io.out.bits.last
  when(outvalid){
    printf( p"DUT: out: valid($outvalid) data(0x${Hexadecimal(outdata)}) keep(0x${Hexadecimal(outkeep)}) last(${outlast})\n" )
  }

  // NOTE: They will fire without losing data 
  when (io.in.fire() && io.out.fire()) {
    // When something immediately goes through the pipeline
    printf( "DUT: io.in/out fire\n" )
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 

    // if(overflow)
    //    if (lastitem)
    //      update backup to 0
    //      update send to read backup by not rotated
    //      stall the ready bit by 1
    //    else
    //      do the normal
    // else
    //    do the normal
    //if you go over, stall next read input by 1
  }
  .elsewhen (io.in.fire()) {
    // When something comes in and nothing goes out yet
    // I don't think this case should happen since you are dependent on the output
    printf( "DUT: io.in fire\n" )
    send_data := (backup_data & lowerMaskBits) | (rotated_data & upperMaskBits)
    backup_data := (0.U & upperMaskBits) | (rotated_data & lowerMaskBits) 
    send_keep := (backup_keep & lowerMaskKeep) | (rotated_keep & upperMaskKeep)
    backup_keep := (0.U & upperMaskKeep) | (rotated_keep & lowerMaskKeep) 
  }
  .elsewhen (io.out.fire()) {
    // When something is output from the pipeline but nothing is put in thus make the next values 0
    printf( "DUT: io.out fire\n" )
    send_data := (backup_data & lowerMaskBits)
    backup_data := 0.U
    send_keep := (backup_keep & lowerMaskKeep)
    backup_keep := 0.U
  }

  printf( "--- DUT DEBUG DONE ---\n\n" )
}

// TODO: THIS DECSRIPTION IS NOT EXACTLY TRUE FIX
/**
 * This functional block is used to align the data read in from the reservation buffer. Specifically, the data read in from memory is aligned by
 * 8B blocks. However, the packet data may start at a different memory address that is not at a multiple of 8B. Thus, this aligner must shift this
 * read in data to send out only the data from the packet (and not any other gibberish data).
 *
 * Ex. 
 *    Start memAddr of Packet Data: 0x4
 *    Data in reservation buffer starts at 0x0 and goes to 8B to 0x7
 *    Aligner fixes this by starting the read from the reservation buffer at 0x4
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
 * Unit test for Aligner class
 *
 * @param testWidth size of the network interface for test
 */
class AlignDataToAddrTest(testWidth: Int = 64) extends UnitTest {
  // Send three packets worth of information
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
  
  val valid = aligner.io.in.valid
  val data = aligner.io.in.bits.data
  val keep = aligner.io.in.bits.keep
  val last = aligner.io.in.bits.last
  val outvalid = aligner.io.out.valid
  val outdata = aligner.io.out.bits.data
  val outkeep = aligner.io.out.bits.keep
  val outlast = aligner.io.out.bits.last
  val outready = aligner.io.out.ready

  printf( "--- TEST DEBUG ------------------------------------------------------------------------\n" )
  when(aligner.io.in.valid){
    printf( p"Test Input:  data(0x${Hexadecimal(data)}) keep(0x${Hexadecimal(keep)}) last($last)\n" )
  }
  val temp1 = outData(outIdx)
  val temp2 = outKeep(outIdx)
  val temp3 = outLast(outIdx)
  when(aligner.io.out.valid){
    printf( p"Test Output: data(0x${Hexadecimal(outdata)}) keep(0x${Hexadecimal(outkeep)}) last($outlast)\n" )
    printf( p"Want Output: data(0x${Hexadecimal(temp1)}) keep(0x${Hexadecimal(temp2)}) last($temp3)\n" )
  }

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
  printf( "--- TEST DEBUG DONE ---\n\n" )

  assert(!aligner.io.out.valid ||
         (compareData( aligner.io.out.bits.data, outData(outIdx), aligner.io.out.bits.keep) &&
         aligner.io.out.bits.keep === outKeep(outIdx) &&
         aligner.io.out.bits.last === outLast(outIdx)),
         "AlignDataToAddrTest: output does not match expected")
}
