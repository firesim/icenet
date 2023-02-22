package icenet

import chisel3._
import chisel3.util._


class StreamChannel(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val keep = UInt((w/8).W)
  val last = Bool()
}

class StreamIO(val w: Int) extends Bundle {
  val in = Flipped(Decoupled(new StreamChannel(w)))
  val out = Decoupled(new StreamChannel(w))

  def flipConnect(other: StreamIO) {
    in <> other.out
    other.in <> out
  }
}
