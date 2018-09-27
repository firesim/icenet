package icenet

import chisel3._

/**
 * Main constants that specify the size of the protocols,
 * the limit periods, etc.
 */
object IceNetConsts {
  // Ethernet constants as given by the IEEE standard
  val ETH_MAX_BYTES = 1520
  val ETH_HEAD_BYTES = 16
  val ETH_MAC_BITS = 48
  val ETH_TYPE_BITS = 16
  val ETH_PAD_BITS = 16

  val IPV4_HEAD_BYTES = 20
  val UDP_HEAD_BYTES = 8

  val RLIMIT_MAX_INC = 256
  val RLIMIT_MAX_PERIOD = 256
  val RLIMIT_MAX_SIZE = 256

  def ETH_BCAST_MAC = ~0.U(ETH_MAC_BITS.W)
}

/**
 * Main constants that govern the network itself
 */
case class IceNetConfig(
  val NET_IF_WIDTH_BITS: Int = 64,
  val NET_LEN_BITS: Int = 16
){
  def NET_FULL_KEEP = ~0.U(NET_IF_WIDTH_BYTES.W)
  def NET_IF_WIDTH_BYTES: Int = NET_IF_WIDTH_BITS / 8
}
