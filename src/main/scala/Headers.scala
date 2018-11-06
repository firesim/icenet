package icenet

import chisel3._
import chisel3.util._
import IceNetConsts._

/**
 * Helper functions for reversing the bytes in a UInt
 */
object NetworkHelpers {
  /**
   * Reverse the bytes of a UInt
   *
   * @param a data to reverse
   * @param n number of bytes to reverse
   */
  def reverse_bytes(a: UInt, n: Int) = {
    val bytes = (0 until n).map(i => a((i + 1) * 8 - 1, i * 8))
    Cat(bytes)
  }
  def htonl(a: UInt) = reverse_bytes(a, 4)
  def ntohl(a: UInt) = reverse_bytes(a, 4)
  def htons(a: UInt) = reverse_bytes(a, 2)
  def ntohs(a: UInt) = reverse_bytes(a, 2)
}

/**
 * Ethernet header class that is of fixed size with conversion functions.
 */
class EthernetHeader extends Bundle {
  val ethType = UInt(ETH_TYPE_BITS.W)
  val srcmac  = UInt(ETH_MAC_BITS.W)
  val dstmac  = UInt(ETH_MAC_BITS.W)
  val padding = UInt(ETH_PAD_BITS.W)

  /**
   * This function converts the ethernet header to a vec of size W chunks
   *
   * @param w size of chunks to break the header in bits
   * @return Vec of headerWords by w
   */
  def toWords(w: Int) = { 
    val headerWords = if(w > (ETH_HEAD_BYTES * 8)) 1 else (ETH_HEAD_BYTES * 8) / w
    //this.asUInt.asTypeOf(Vec(headerWords, UInt(w.W)))
    if (headerWords == 1){
      (this.asUInt() << (w - ETH_HEAD_BYTES*8)).asTypeOf(Vec(headerWords, UInt(w.W)))
    }
    else {
      this.asUInt().asTypeOf(Vec(headerWords, UInt(w.W)))
    }
  }

  /**
   * This function converts from a Vec with inner widths of W to an ethernet header
   *
   * @param w size of chunks to remake the header in bits
   * @return Ethernet header
   */
  def fromWords(words: Seq[UInt], w: Int) = {
    val headerWords = if(w > (ETH_HEAD_BYTES * 8)) 1 else (ETH_HEAD_BYTES * 8) / w
    //Cat(words.take(headerWords).reverse).asTypeOf(this)

    // note: if w > ETH_HEAD_BYTES*8 then the header will be taken from the most significant bits
    if (headerWords == 1){
      (Cat(words.take(headerWords).reverse) >> (w - ETH_HEAD_BYTES*8)).asTypeOf(this)
    }
    else {
      Cat(words.take(headerWords).reverse).asTypeOf(this)
    }


  }

  override def cloneType = (new EthernetHeader).asInstanceOf[this.type]
}

/**
 * Companion object to create the EthernetHeader and connect it properly
 */
object EthernetHeader {
  def apply(dstmac: UInt, srcmac: UInt, ethType: UInt) = {
    val header = Wire(new EthernetHeader)
    header.dstmac := dstmac
    header.srcmac := srcmac
    header.ethType := ethType
    header.padding := DontCare
    header
  }
}

/**
 * Implementation of a IPv4 Header
 */
class IPv4Header extends Bundle {
  val dest_ip = UInt(32.W)
  val source_ip = UInt(32.W)
  val header_checksum = UInt(16.W)
  val protocol = UInt(8.W)
  val ttl = UInt(8.W)
  val frag_off2 = UInt(8.W)
  val flags = UInt(3.W)
  val frag_off1 = UInt(5.W)
  val ident = UInt(16.W)
  val total_length = UInt(16.W)
  val dcsp = UInt(6.W)
  val ecn = UInt(2.W)
  val version = UInt(4.W)
  val ihl = UInt(4.W)
}

/**
 * Implementation of a UDPHeader
 */
class UDPHeader extends Bundle {
  val checksum = UInt(16.W)
  val length = UInt(16.W)
  val dest_port = UInt(16.W)
  val source_port = UInt(16.W)
}
