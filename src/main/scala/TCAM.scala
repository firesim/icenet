package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import testchipip.TLHelper

class TCAMMatchIO(n: Int, dataBits: Int) extends Bundle {
  val addrBits = log2Ceil(n)
  val data = Output(UInt(dataBits.W))
  val addr = Input(UInt(addrBits.W))
  val found = Input(Bool())

}

class TCAM(address: BigInt, val n: Int, val dataBits: Int, val nPorts: Int)
    (implicit p: Parameters) extends LazyModule {
  val addrBits = log2Ceil(n)
  val byteAddrBits = log2Ceil(dataBits / 8)
  val beatBytes = 1 << byteAddrBits
  val addrMask = (1 << (1 + addrBits + byteAddrBits)) - 1

  val node = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
    address = Seq(AddressSet(address, addrMask)),
    regionType = RegionType.PUT_EFFECTS,
    supportsGet = TransferSizes(1, beatBytes),
    supportsPutFull = TransferSizes(1, beatBytes)))

  lazy val module = new TCAMModule(this)
}

class TCAMModule(outer: TCAM) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val tcam = Flipped(
      Vec(outer.nPorts, new TCAMMatchIO(outer.n, outer.dataBits)))
  })

  val (tl, edge) = outer.node.in(0)

  val dataArr = Reg(Vec(outer.n, UInt(outer.dataBits.W)))
  val maskArr = Reg(Vec(outer.n, UInt(outer.dataBits.W)))

  val acq = Queue(tl.a, 1)
  val regsel = acq.bits.address(outer.addrBits + outer.byteAddrBits)
  val wordaddr = acq.bits.address(
    outer.addrBits + outer.byteAddrBits - 1, outer.byteAddrBits)

  tl.d.valid := acq.valid
  acq.ready := tl.d.ready
  tl.d.bits := edge.AccessAck(acq.bits, 0.U)
  tl.d.bits.opcode := Mux(edge.hasData(acq.bits),
    TLMessages.AccessAck, TLMessages.AccessAckData)
  tl.d.bits.data := Mux(regsel, maskArr(wordaddr), dataArr(wordaddr))

  when (acq.fire && edge.hasData(acq.bits)) {
    when (regsel) {
      maskArr(wordaddr) := acq.bits.data(outer.dataBits - 1, 0)
    } .otherwise {
      dataArr(wordaddr) := acq.bits.data(outer.dataBits - 1, 0)
    }
  }

  io.tcam.foreach { m =>
    val matches = dataArr.zip(maskArr).map { case (data, mask) =>
      !((m.data ^ data) & mask).orR
    }
    m.addr := PriorityEncoder(matches)
    m.found := matches.reduce(_ || _)
  }
}
