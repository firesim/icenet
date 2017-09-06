package icenet

import chisel3._
import chisel3.util._

object IceNetConsts {
  val NET_IF_WIDTH = 64
  val NET_IF_BYTES = NET_IF_WIDTH/8
  val NET_LEN_BITS = 16
  val ETH_MAX_WORDS = 190
  val ETH_HEAD_WORDS = 2
  val ETH_MAC_BITS = 48
  val ETH_TYPE_BITS = 16
  val ETH_PAD_BITS = 16

  def NET_FULL_KEEP = ~0.U(NET_IF_BYTES.W)
  def ETH_BCAST_MAC = ~0.U(ETH_MAC_BITS.W)
}

import IceNetConsts._

class EthernetHeader extends Bundle {
  val ethType = UInt(ETH_TYPE_BITS.W)
  val srcmac  = UInt(ETH_MAC_BITS.W)
  val dstmac  = UInt(ETH_MAC_BITS.W)
  val padding = UInt(ETH_PAD_BITS.W)

  def toWords(dummy: Int = 0) =
    Vec(ETH_HEAD_WORDS, UInt(NET_IF_WIDTH.W)).fromBits(this.asUInt)

  def fromWords(words: Seq[UInt]) =
    this.fromBits(Cat(words.take(ETH_HEAD_WORDS).reverse))
}

object EthernetHeader {
  def apply(dstmac: UInt, srcmac: UInt, ethType: UInt) = {
    val header = Wire(new EthernetHeader)
    header.dstmac := dstmac
    header.srcmac := srcmac
    header.ethType := ethType
    header
  }
}
