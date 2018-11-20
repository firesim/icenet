package icenet

import chisel3._

/**
 * Main constants that specify the size of the protocols,
 * the limit periods, etc.
 */
object IceNetConsts {
  // Assumed freq in gHz
  val PROC_SPEED = 3.2

  // Ethernet constants as given by the IEEE standard
  val ETH_MAX_BYTES = 1520
  val ETH_HEAD_BYTES = 16
  val ETH_MAC_BITS = 48
  val ETH_TYPE_BITS = 16
  val ETH_PAD_BITS = 16

  // Size of specific header types
  val IPV4_HEAD_BYTES = 20
  val UDP_HEAD_BYTES = 8

  def ETH_BCAST_MAC = ~0.U(ETH_MAC_BITS.W)
}

/**
 * Main constants that govern the network itself. This is created on a class by class
 * basis using the NICConfig class parameters in NIC.scala. This allows the configuration
 * parameters to be passed around multiple different classes that may not have access to NICConfig.
 * 
 * @param NET_IF_WIDTH_BITS flit size in bits
 * @param NET_LEN_BITS size in bits of the network packet length
 */
case class IceNetConfig(
  val NET_IF_WIDTH_BITS: Int = 64,
  val NET_LEN_BITS: Int = 16,
){
  def NET_IF_WIDTH_BYTES: Int = NET_IF_WIDTH_BITS / 8 // this is the flit size in bytes
  def NET_FULL_KEEP = ~0.U(NET_IF_WIDTH_BYTES.W) // this is a bytemask to indicate which bytes should be kept

  // these should all be larger than the max network bandwidth in GBps
  def RLIMIT_MAX_INC: Int = (NET_IF_WIDTH_BITS.asInstanceOf[Double] * IceNetConsts.PROC_SPEED).asInstanceOf[Int]
  def RLIMIT_MAX_PERIOD: Int = (NET_IF_WIDTH_BITS.asInstanceOf[Double] * IceNetConsts.PROC_SPEED).asInstanceOf[Int]
  def RLIMIT_MAX_SIZE: Int = (NET_IF_WIDTH_BITS.asInstanceOf[Double] * IceNetConsts.PROC_SPEED).asInstanceOf[Int]
}
