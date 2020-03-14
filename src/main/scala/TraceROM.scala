package icenet

import chisel3._
import chisel3.util._
import testchipip.StreamChannel
import IceNetConsts.NET_IF_WIDTH

class TraceROM extends BlackBox with HasBlackBoxResource{
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val stream = Decoupled(new StreamChannel(NET_IF_WIDTH))
    val macAddr = Output(UInt(48.W))
    val length = Output(UInt(32.W))
  })

  addResource("/vsrc/TraceROM.v")
  addResource("/csrc/TraceROM.cc")
}
