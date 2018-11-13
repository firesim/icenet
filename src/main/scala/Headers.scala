package icenet

import chisel3._
import chisel3.util._
import IceNetConsts._

object NetworkHelpers {
  def reverse_bytes(a: UInt, n: Int) = {
    val bytes = (0 until n).map(i => a((i + 1) * 8 - 1, i * 8))
    Cat(bytes)
  }
  def htonl(a: UInt) = reverse_bytes(a, 4)
  def ntohl(a: UInt) = reverse_bytes(a, 4)
  def htons(a: UInt) = reverse_bytes(a, 2)
  def ntohs(a: UInt) = reverse_bytes(a, 2)
}

class EthernetHeader extends Bundle {
  val ethType = UInt(ETH_TYPE_BITS.W)
  val srcmac  = UInt(ETH_MAC_BITS.W)
  val dstmac  = UInt(ETH_MAC_BITS.W)
  val padding = UInt(ETH_PAD_BITS.W)

  def toWords(w: Int = NET_IF_WIDTH) =
    Vec(ETH_HEAD_BYTES * 8 / w, UInt(w.W)).fromBits(this.asUInt)

  def fromWords(words: Seq[UInt], w: Int = NET_IF_WIDTH) = {
    val headerWords = ETH_HEAD_BYTES * 8 / w
    this.fromBits(Cat(words.take(headerWords).reverse))
  }
}

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

class UDPHeader extends Bundle {
  val checksum = UInt(16.W)
  val length = UInt(16.W)
  val dest_port = UInt(16.W)
  val source_port = UInt(16.W)
}
