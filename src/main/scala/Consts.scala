package icenet

import chisel3._
import chisel3.util._

object IceNetConsts {
  val NET_IF_WIDTH = 64
  val NET_LEN_BITS = 16
  val ETH_MAX_WORDS = 190
  val ETH_HEAD_WORDS = 2
  val ETH_MAC_BITS = 48
  val ETH_TYPE_BITS = 16
  val ETH_PAD_BITS = 16
}

import IceNetConsts._

class EthernetHeader extends Bundle {
  val ethType = UInt(ETH_TYPE_BITS.W)
  val srcmac  = UInt(ETH_MAC_BITS.W)
  val dstmac  = UInt(ETH_MAC_BITS.W)
  val padding = UInt(ETH_PAD_BITS.W)

  private val headerWords =
    (ETH_TYPE_BITS + 2 * ETH_MAC_BITS + ETH_PAD_BITS) / NET_IF_WIDTH

  def toWords(dummy: Int = 0) =
    Vec(headerWords, UInt(NET_IF_WIDTH.W)).fromBits(this.asUInt)

  def fromWords(words: Seq[UInt]) =
    this.fromBits(Cat(words.take(headerWords).reverse))
}
