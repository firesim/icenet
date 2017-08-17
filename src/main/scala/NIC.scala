package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.coreplex.HasSystemBus
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.rocket.PAddrBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.TwoWayCounter
import testchipip.{StreamIO, StreamChannel, SeqQueue}
import scala.util.Random
import IceNetConsts._

case class NICConfig(inBufPackets: Int = 2)

case object NICKey extends Field[NICConfig]

class IceNicSendIO extends Bundle {
  val req = Decoupled(UInt(NET_IF_WIDTH.W))
  val comp = Flipped(Decoupled(Bool()))
}

class IceNicRecvIO extends Bundle {
  val req = Decoupled(UInt(NET_IF_WIDTH.W))
  val comp = Flipped(Decoupled(UInt(NET_LEN_BITS.W)))
}

trait IceNicControllerBundle extends Bundle {
  val send = new IceNicSendIO
  val recv = new IceNicRecvIO
  val macAddr = Input(UInt(ETH_MAC_BITS.W))
}

trait IceNicControllerModule extends Module with HasRegMap {
  val io: IceNicControllerBundle

  val sendCompDown = Wire(init = false.B)

  val qDepth = 10
  // hold (len, addr) of packets that we need to send out
  val sendReqQueue = Module(new Queue(UInt(NET_IF_WIDTH.W), qDepth))
  // hold addr of buffers we can write received packets into
  val recvReqQueue = Module(new Queue(UInt(NET_IF_WIDTH.W), qDepth))
  // count number of sends completed
  val sendCompCount = TwoWayCounter(io.send.comp.fire(), sendCompDown, qDepth)
  // hold length of received packets
  val recvCompQueue = Module(new Queue(UInt(NET_LEN_BITS.W), qDepth))

  val sendCompValid = sendCompCount > 0.U

  io.send.req <> sendReqQueue.io.deq
  io.recv.req <> recvReqQueue.io.deq
  io.send.comp.ready := sendCompCount < 10.U
  recvCompQueue.io.enq <> io.recv.comp

  interrupts(0) := sendCompValid || recvCompQueue.io.deq.valid

  val sendReqAvail = (qDepth.U - sendReqQueue.io.count)
  val recvReqAvail = (qDepth.U - recvReqQueue.io.count)

  def sendCompRead = (ready: Bool) => {
    sendCompDown := sendCompValid && ready
    (sendCompValid, true.B)
  }

  regmap(
    0x00 -> Seq(RegField.w(NET_IF_WIDTH, sendReqQueue.io.enq)),
    0x08 -> Seq(RegField.w(NET_IF_WIDTH, recvReqQueue.io.enq)),
    0x10 -> Seq(RegField.r(1, sendCompRead)),
    0x12 -> Seq(RegField.r(NET_LEN_BITS, recvCompQueue.io.deq)),
    0x14 -> Seq(
      RegField.r(4, qDepth.U - sendReqQueue.io.count),
      RegField.r(4, qDepth.U - recvReqQueue.io.count),
      RegField.r(4, sendCompCount),
      RegField.r(4, recvCompQueue.io.count)),
    0x18 -> Seq(RegField.r(ETH_MAC_BITS, io.macAddr)))
}

case class IceNicControllerParams(address: BigInt, beatBytes: Int)

/*
 * Take commands from the CPU over TL2, expose as Queues
 */
class IceNicController(c: IceNicControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "ice-nic", Seq("ucbbar,ice-nic"),
    interrupts = 1, beatBytes = c.beatBytes)(
      new TLRegBundle(c, _)    with IceNicControllerBundle)(
      new TLRegModule(c, _, _) with IceNicControllerModule)

/*
 * Send frames out
 */
class IceNicSendPath(implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "ice-nic-send", sourceId = IdRange(0, 1)))
  lazy val module = new IceNicSendPathModule(this)
}

class IceNicSendPathModule(outer: IceNicSendPath)
    extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val tl = outer.node.bundleOut
    val send = Flipped(new IceNicSendIO)
    val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
  })

  val tl = io.tl(0)
  val beatBytes = tl.params.dataBits / 8
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = p(PAddrBits) - byteAddrBits
  val lenBits = NET_LEN_BITS - byteAddrBits
  val midPoint = NET_IF_WIDTH - NET_LEN_BITS
  val packlen = io.send.req.bits(NET_IF_WIDTH - 1, midPoint)
  val packaddr = io.send.req.bits(midPoint - 1, 0)

  // we allow one TL request at a time to avoid tracking
  val s_idle :: s_read :: s_send :: s_comp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val sendaddr = Reg(UInt(addrBits.W))
  val sendlen  = Reg(UInt(lenBits.W))

  val edge = outer.node.edgesOut(0)
  val grantqueue = Queue(tl.d, 1)

  io.send.req.ready := state === s_idle
  tl.a.valid := state === s_read
  tl.a.bits := edge.Get(
    fromSource = 0.U,
    toAddress = sendaddr << byteAddrBits.U,
    lgSize = byteAddrBits.U)._2
  io.out.valid := grantqueue.valid && state === s_send
  io.out.bits.data := grantqueue.bits.data
  io.out.bits.last := sendlen === 0.U
  grantqueue.ready := io.out.ready && state === s_send
  io.send.comp.valid := state === s_comp
  io.send.comp.bits := true.B

  when (io.send.req.fire()) {
    sendaddr := packaddr >> byteAddrBits.U
    sendlen  := packlen  >> byteAddrBits.U
    state := s_read

    assert(packaddr(byteAddrBits-1,0) === 0.U &&
           packlen(byteAddrBits-1,0)  === 0.U,
           s"NIC send address and length must be aligned to ${beatBytes} bytes")
  }

  when (tl.a.fire()) {
    sendaddr := sendaddr + 1.U
    sendlen  := sendlen - 1.U
    state := s_send
  }

  when (io.out.fire()) {
    state := Mux(sendlen === 0.U, s_comp, s_read)
  }

  when (io.send.comp.fire()) {
    state := s_idle
  }
}

class IceNicWriter(val nXacts: Int)(implicit p: Parameters)
    extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "ice-nic-recv", sourceId = IdRange(0, nXacts)))
  lazy val module = new IceNicWriterModule(this)
}

class IceNicWriterModule(outer: IceNicWriter)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val tl = outer.node.bundleOut
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
  })

  val tl = io.tl(0)
  val edge = outer.node.edgesOut(0)
  val beatBytes = tl.params.dataBits / 8
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = p(PAddrBits) - byteAddrBits

  val s_idle :: s_data :: s_complete :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val base_addr = RegInit(0.U(addrBits.W))
  val idx = RegInit(0.U(addrBits.W))
  val addr_merged = base_addr + idx

  val xact_busy = RegInit(0.U(outer.nXacts.W))
  val xact_onehot = PriorityEncoderOH(~xact_busy)
  val can_send = !xact_busy.andR

  xact_busy := (xact_busy | Mux(tl.a.fire(), xact_onehot, 0.U)) &
                  ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  io.recv.req.ready := state === s_idle
  tl.a.valid := (state === s_data && io.in.valid) && can_send
  tl.a.bits := edge.Put(
    fromSource = OHToUInt(xact_onehot),
    toAddress = addr_merged << byteAddrBits.U,
    lgSize = byteAddrBits.U,
    data = io.in.bits.data)._2
  tl.d.ready := xact_busy.orR
  io.in.ready := state === s_data && can_send && tl.a.ready
  io.recv.comp.valid := state === s_complete && !xact_busy.orR
  io.recv.comp.bits := idx << byteAddrBits.U

  when (io.recv.req.fire()) {
    idx := 0.U
    base_addr := io.recv.req.bits >> byteAddrBits.U
    state := s_data
  }

  when (tl.a.fire()) {
    idx := idx + 1.U
    when (io.in.bits.last) { state := s_complete }
  }

  when (io.recv.comp.fire()) { state := s_idle }
}

/*
 * Recv frames
 */
class IceNicRecvPath(nXacts: Int)(implicit p: Parameters)
    extends LazyModule {
  val writer = LazyModule(new IceNicWriter(nXacts))
  val node = TLOutputNode()
  node := writer.node
  lazy val module = new IceNicRecvPathModule(this)
}

class IceNicRecvPathModule(outer: IceNicRecvPath)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val tl = outer.node.bundleOut // dma mem port
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH))) // input stream 
  })

  val config = p(NICKey)
  val buffer = Module(new NetworkPacketBuffer(config.inBufPackets))
  buffer.io.stream.in <> io.in

  val writer = outer.writer.module
  writer.io.in <> buffer.io.stream.out
  writer.io.recv <> io.recv
}

class NICIO extends StreamIO(NET_IF_WIDTH) {
  val macAddr = Input(UInt(ETH_MAC_BITS.W))

  override def cloneType = (new NICIO).asInstanceOf[this.type]
}

/* 
 * A simple NIC
 *
 * Expects ethernet frames (see below), but uses a custom transport 
 * (see ExtBundle)
 * 
 * Ethernet Frame format:
 *   8 bytes    |  6 bytes  |  6 bytes    | 2 bytes  | 46-1500B | 4 bytes
 * Preamble/SFD | Dest Addr | Source Addr | Type/Len | Data     | CRC
 * Gen by NIC   | ------------- from/to CPU --------------------| Gen by NIC
 *
 * For now, we elide the Gen by NIC components since we're talking to a 
 * custom network.
 */
class IceNIC(address: BigInt, beatBytes: Int = 8, nXacts: Int = 8)
    (implicit p: Parameters) extends LazyModule {

  val control = LazyModule(new IceNicController(
    IceNicControllerParams(address, beatBytes)))
  val sendPath = LazyModule(new IceNicSendPath)
  val recvPath = LazyModule(new IceNicRecvPath(nXacts))

  val mmionode = TLInputNode()
  val dmanode = TLOutputNode()
  val intnode = IntOutputNode()

  control.node := mmionode
  dmanode := sendPath.node
  dmanode := recvPath.node
  intnode := control.intnode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val tlout = dmanode.bundleOut // move packets in/out of mem
      val tlin = mmionode.bundleIn  // commands from cpu
      val ext = new NICIO
      val interrupt = intnode.bundleOut
    })

    sendPath.module.io.send <> control.module.io.send
    recvPath.module.io.recv <> control.module.io.recv

    // connect externally
    recvPath.module.io.in <> io.ext.in
    io.ext.out <> sendPath.module.io.out
    control.module.io.macAddr := io.ext.macAddr
  }
}

class SimNetwork extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val net = Flipped(new NICIO)
  })
}

trait HasPeripheryIceNIC extends HasSystemBus {
  private val address = BigInt(0x10016000)

  val simplenic = LazyModule(new IceNIC(address, sbus.beatBytes))
  simplenic.mmionode := sbus.toVariableWidthSlaves
  sbus.fromSyncPorts() :=* simplenic.dmanode
  ibus.fromSync := simplenic.intnode
}

trait HasPeripheryIceNICModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryIceNIC
  val net = IO(new NICIO)

  net <> outer.simplenic.module.io.ext

  def connectNicLoopback(qDepth: Int = 64) {
    net.in <> Queue(net.out, qDepth)
  }

  def connectSimNetwork(dummy: Int = 0) {
    val sim = Module(new SimNetwork)
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.net <> net
  }
}

