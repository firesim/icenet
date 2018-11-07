package icenet

import chisel3._
import chisel3.util._
import scala.math.max
import testchipip.StreamChannel
import IceNetConsts._

/**
 * Helper class to create packets from input data/keep and packet lengths
 *
 * @param lengths lengths of each packet to generate from the data 
 * @param genData data to put into packets 
 * @param genKeep keep associated with the data
 * @param netConfig configuration settings 
 */
class PacketGen(lengths: Seq[Int], genData: Seq[BigInt], genKeep: Seq[BigInt], netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val out = Decoupled(new StreamChannel(netConfig.NET_IF_WIDTH_BITS))
  })

  val maxLength = lengths.reduce(max(_, _))
  val totalLength = lengths.reduce(_ + _)
  val lengthVec = VecInit(lengths.map(_.U))
  val dataVec = VecInit(genData.map(_.U(netConfig.NET_IF_WIDTH_BITS.W)))
  val keepVec = VecInit(genKeep.map(_.U(netConfig.NET_IF_WIDTH_BYTES.W)))

  require(totalLength == genData.size)

  val pktIdx = Reg(UInt(log2Ceil(lengths.size).W))
  val pktOffset = Reg(UInt(log2Ceil(maxLength).W))
  val dataIdx = Reg(UInt(log2Ceil(totalLength).W))
  val sending = RegInit(false.B)

  when (!sending && io.start) {
    sending := true.B
    pktIdx := 0.U
    pktOffset := 0.U
    dataIdx := 0.U
  }

  when (io.out.fire()) {
    dataIdx := dataIdx + 1.U
    pktOffset := pktOffset + 1.U
    when (io.out.bits.last) {
      pktIdx := pktIdx + 1.U
      pktOffset := 0.U
      when (pktIdx === (lengths.size - 1).U) {
        sending := false.B
      }
    }
  }

  io.out.valid := sending
  io.out.bits.data := dataVec(dataIdx)
  io.out.bits.keep := keepVec(dataIdx)
  io.out.bits.last := pktOffset === lengthVec(pktIdx) - 1.U
}

/**
 * Helper class to check the data input
 *
 * @param checkData the required data to check
 * @param checkKeep the keep bytemask associated with the data 
 * @param checkLast the last indicator for the bytes sent 
 * @param netConfig the configuration settings 
 */
class PacketCheck(
    checkData: Seq[BigInt],
    checkKeep: Seq[BigInt],
    checkLast: Seq[Boolean],
    netConfig: IceNetConfig) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(netConfig.NET_IF_WIDTH_BITS)))
    val finished = Output(Bool())
  })

  val checkDataVec = VecInit(checkData.map(_.U(netConfig.NET_IF_WIDTH_BITS.W)))
  val checkKeepVec = VecInit(checkKeep.map(_.U(netConfig.NET_IF_WIDTH_BYTES.W)))
  val checkLastVec = VecInit(checkLast.map(_.B))

  val (checkIdx, checkDone) = Counter(io.in.fire(), checkDataVec.length)

  val finished = RegInit(false.B)

  io.in.ready := !finished
  io.finished := finished

  when (checkDone) { finished := true.B }

  def compareData(a: UInt, b: UInt, keep: UInt) = {
    val bitmask = FillInterleaved(8, keep) //Expand bitmask to bytemask
    (a & bitmask) === (b & bitmask)
  }

  //when (io.in.valid){
  //  printf("Packet Check ----\n")
  //  printf("  in: data(0x%x) keep(0x%x) last(0x%x)\n", io.in.bits.data, io.in.bits.keep, io.in.bits.last)
  //  printf("want: data(0x%x) keep(0x%x) last(0x%x)\n", checkDataVec(checkIdx), checkKeepVec(checkIdx), checkLastVec(checkIdx))
  //  printf("Packet Check Done ----\n")
  //}

  // Everything should match (data, keep and last bits)
  assert(!io.in.valid ||
    (compareData(io.in.bits.data, checkDataVec(checkIdx), io.in.bits.keep) &&
      io.in.bits.keep === checkKeepVec(checkIdx) &&
      io.in.bits.last === checkLastVec(checkIdx)),
    "PacketCheck: input does not match")

}
