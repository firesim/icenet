package icenet

import chisel3._
import chisel3.util._
import testchipip.{StreamChannel}
import IceNetConsts._

class PacketDistributor(nOut: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val inLength = Input(UInt(16.W))
    val out = Vec(nOut, Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val outLength = Output(Vec(nOut, UInt(16.W)))
  })

  val curRoute = RegInit(0.U(log2Ceil(nOut).W))
  val started = RegInit(false.B)

  io.in.ready := false.B

  for (i <- 0 until nOut) {
    val me = curRoute === i.U
    io.out(i).valid := io.in.valid && curRoute === i.U
    io.out(i).bits := io.in.bits
    io.outLength(i) := io.inLength
    when (me) { io.in.ready := io.out(i).ready }
  }

  val outSkip = !started && io.in.valid && !io.in.ready
  val outStart = !started && io.in.fire()
  val outFinish = io.in.fire() && io.in.bits.last

  def wrapInc(route: UInt) = Mux(route === (nOut-1).U, 0.U, route + 1.U)

  when (outSkip || outFinish) { curRoute := wrapInc(curRoute) }
  when (outStart) { started := true.B }
  when (outFinish) { started := false.B }
}
