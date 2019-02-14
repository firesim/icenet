package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import testchipip.{StreamIO, StreamChannel, TLHelper}
import IceNetConsts._

case class NICConfig(
  inBufPackets: Int = 2,
  outBufFlits: Int = 2 * ETH_MAX_BYTES / NET_IF_BYTES,
  nMemXacts: Int = 8,
  maxAcquireBytes: Int = 64,
  ctrlQueueDepth: Int = 10)

case object NICKey extends Field[NICConfig]

trait HasNICParameters {
  implicit val p: Parameters
  val nicExternal = p(NICKey)
  val inBufPackets = nicExternal.inBufPackets
  val outBufFlits = nicExternal.outBufFlits
  val nMemXacts = nicExternal.nMemXacts
  val maxAcquireBytes = nicExternal.maxAcquireBytes
  val ctrlQueueDepth = nicExternal.ctrlQueueDepth
  val creditTrackerParams = nicExternal.creditTracker
}

abstract class NICLazyModule(implicit p: Parameters)
  extends LazyModule with HasNICParameters

abstract class NICModule(implicit val p: Parameters)
  extends Module with HasNICParameters

abstract class NICBundle(implicit val p: Parameters)
  extends Bundle with HasNICParameters

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

trait IceNicControllerModule extends HasRegMap with HasNICParameters {
  implicit val p: Parameters
  val io: IceNicControllerBundle

  val sendCompDown = WireInit(false.B)

  val qDepth = ctrlQueueDepth
  require(qDepth < (1 << 8))

  def queueCount[T <: Data](qio: QueueIO[T], depth: Int): UInt =
    TwoWayCounter(qio.enq.fire(), qio.deq.fire(), depth)

  // hold (len, addr) of packets that we need to send out
  val sendReqQueue = Module(new HellaQueue(qDepth)(UInt(NET_IF_WIDTH.W)))
  val sendReqCount = queueCount(sendReqQueue.io, qDepth)
  // hold addr of buffers we can write received packets into
  val recvReqQueue = Module(new HellaQueue(qDepth)(UInt(NET_IF_WIDTH.W)))
  val recvReqCount = queueCount(recvReqQueue.io, qDepth)
  // count number of sends completed
  val sendCompCount = TwoWayCounter(io.send.comp.fire(), sendCompDown, qDepth)
  // hold length of received packets
  val recvCompQueue = Module(new HellaQueue(qDepth)(UInt(NET_LEN_BITS.W)))
  val recvCompCount = queueCount(recvCompQueue.io, qDepth)

  val sendCompValid = sendCompCount > 0.U
  val intMask = RegInit(0.U(2.W))

  io.send.req <> sendReqQueue.io.deq
  io.recv.req <> recvReqQueue.io.deq
  io.send.comp.ready := sendCompCount < qDepth.U
  recvCompQueue.io.enq <> io.recv.comp

  interrupts(0) := sendCompValid && intMask(0)
  interrupts(1) := recvCompQueue.io.deq.valid && intMask(1)

  val sendReqSpace = (qDepth.U - sendReqCount)
  val recvReqSpace = (qDepth.U - recvReqCount)

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
      RegField.r(8, sendReqSpace),
      RegField.r(8, recvReqSpace),
      RegField.r(8, sendCompCount),
      RegField.r(8, recvCompCount)),
    0x18 -> Seq(RegField.r(ETH_MAC_BITS, io.macAddr)),
    0x20 -> Seq(RegField(2, intMask)))
}

case class IceNicControllerParams(address: BigInt, beatBytes: Int)

/*
 * Take commands from the CPU over TL2, expose as Queues
 */
class IceNicController(c: IceNicControllerParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "ice-nic", Seq("ucbbar,ice-nic"),
    interrupts = 2, beatBytes = c.beatBytes)(
      new TLRegBundle(c, _)    with IceNicControllerBundle)(
      new TLRegModule(c, _, _) with IceNicControllerModule)

class IceNicSendPath(nInputTaps: Int = 0)(implicit p: Parameters)
    extends NICLazyModule {
  val reader = LazyModule(new StreamReader(
    nMemXacts, outBufFlits, maxAcquireBytes))
  val node = reader.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val send = Flipped(new IceNicSendIO)
      val tap = (nInputTaps > 0).option(
        Flipped(Vec(nInputTaps, Decoupled(new StreamChannel(NET_IF_WIDTH)))))
      val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
      val rlimit = Input(new RateLimiterSettings)
    })

    val readreq = reader.module.io.req
    io.send.req.ready := readreq.ready
    readreq.valid := io.send.req.valid
    readreq.bits.address := io.send.req.bits(47, 0)
    readreq.bits.length  := io.send.req.bits(62, 48)
    readreq.bits.partial := io.send.req.bits(63)
    io.send.comp <> reader.module.io.resp

    val unlimitedOut = if (nInputTaps > 0) {
      val arb = Module(new HellaPeekingArbiter(
        new StreamChannel(NET_IF_WIDTH), 1 + nInputTaps,
        (chan: StreamChannel) => chan.last, rr = true))
      arb.io.in <> (reader.module.io.out +: io.tap.get)
      arb.io.out
    } else { reader.module.io.out }

    val limiter = Module(new RateLimiter(new StreamChannel(NET_IF_WIDTH)))
    limiter.io.in <> unlimitedOut
    limiter.io.settings := io.rlimit
    io.out <> limiter.io.out
  }
}

class IceNicWriter(implicit p: Parameters) extends NICLazyModule {
  val writer = LazyModule(new StreamWriter(nMemXacts, maxAcquireBytes))
  val node = writer.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val recv = Flipped(new IceNicRecvIO)
      val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
      val length = Input(UInt(NET_LEN_BITS.W))
    })

    val streaming = RegInit(false.B)
    val byteAddrBits = log2Ceil(NET_IF_BYTES)
    val helper = DecoupledHelper(
      io.recv.req.valid,
      writer.module.io.req.ready,
      io.in.valid, !streaming)

    writer.module.io.req.valid := helper.fire(writer.module.io.req.ready)
    writer.module.io.req.bits.address := io.recv.req.bits
    writer.module.io.req.bits.length := io.length
    io.recv.req.ready := helper.fire(io.recv.req.valid)

    writer.module.io.in.valid := io.in.valid && streaming
    writer.module.io.in.bits := io.in.bits
    io.in.ready := writer.module.io.in.ready && streaming

    io.recv.comp <> writer.module.io.resp

    when (io.recv.req.fire()) { streaming := true.B }
    when (io.in.fire() && io.in.bits.last) { streaming := false.B }
  }
}

/*
 * Recv frames
 */
class IceNicRecvPath(val tapFuncs: Seq[EthernetHeader => Bool] = Nil)
    (implicit p: Parameters) extends LazyModule {
  val writer = LazyModule(new IceNicWriter)
  val node = TLIdentityNode()
  node := writer.node
  lazy val module = new IceNicRecvPathModule(this)
}

class IceNicRecvPathModule(outer: IceNicRecvPath)
    extends LazyModuleImp(outer) with HasNICParameters {
  val io = IO(new Bundle {
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH))) // input stream 
    val tap = outer.tapFuncs.nonEmpty.option(
      Vec(outer.tapFuncs.length, Decoupled(new StreamChannel(NET_IF_WIDTH))))
  })

  val buffer = Module(new NetworkPacketBuffer(inBufPackets))
  buffer.io.stream.in <> io.in

  val writer = outer.writer.module
  writer.io.length := buffer.io.length
  writer.io.recv <> io.recv
  writer.io.in <> (if (outer.tapFuncs.nonEmpty) {
    val tap = Module(new NetworkTap(outer.tapFuncs))
    tap.io.inflow <> buffer.io.stream.out
    io.tap.get <> tap.io.tapout
    tap.io.passthru
  } else { buffer.io.stream.out })
}

class NICIO extends StreamIO(NET_IF_WIDTH) {
  val macAddr = Input(UInt(ETH_MAC_BITS.W))
  val rlimit = Input(new RateLimiterSettings)

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
class IceNIC(address: BigInt, beatBytes: Int = 8,
    tapOutFuncs: Seq[EthernetHeader => Bool] = Nil,
    nInputTaps: Int = 0)
    (implicit p: Parameters) extends NICLazyModule {

  val control = LazyModule(new IceNicController(
    IceNicControllerParams(address, beatBytes)))
  val sendPath = LazyModule(new IceNicSendPath(nInputTaps))
  val recvPath = LazyModule(new IceNicRecvPath(tapOutFuncs))

  val mmionode = TLIdentityNode()
  val dmanode = TLIdentityNode()
  val intnode = control.intnode

  control.node := TLAtomicAutomata() := mmionode
  dmanode := TLWidthWidget(NET_IF_BYTES) := sendPath.node
  dmanode := TLWidthWidget(NET_IF_BYTES) := recvPath.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ext = new NICIO
      val tapOut = tapOutFuncs.nonEmpty.option(
        Vec(tapOutFuncs.length, Decoupled(new StreamChannel(NET_IF_WIDTH))))
      val tapIn = (nInputTaps > 0).option(
        Flipped(Vec(nInputTaps, Decoupled(new StreamChannel(NET_IF_WIDTH)))))
    })

    sendPath.module.io.send <> control.module.io.send
    recvPath.module.io.recv <> control.module.io.recv

    // connect externally
    recvPath.module.io.in <> io.ext.in
    io.ext.out <> sendPath.module.io.out

    control.module.io.macAddr := io.ext.macAddr
    sendPath.module.io.rlimit := io.ext.rlimit

    io.tapOut.zip(recvPath.module.io.tap).foreach {
      case (a, b) => a <> b
    }
    sendPath.module.io.tap.zip(io.tapIn).foreach {
      case (a, b) => a <> b
    }
  }
}

class SimNetwork extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val net = Flipped(new NICIO)
  })
}

trait HasPeripheryIceNIC  { this: BaseSubsystem =>
  private val address = BigInt(0x10016000)
  private val portName = "Ice-NIC"

  val icenic = LazyModule(new IceNIC(address, sbus.beatBytes))
  sbus.toVariableWidthSlave(Some(portName)) { icenic.mmionode }
  sbus.fromPort(Some(portName))() :=* icenic.dmanode
  ibus.fromSync := icenic.intnode
}

trait HasPeripheryIceNICModuleImp extends LazyModuleImp {
  val outer: HasPeripheryIceNIC
  val net = IO(new NICIO)

  net <> outer.icenic.module.io.ext

  def connectNicLoopback(qDepth: Int = 64) {
    net.in <> Queue(net.out, qDepth)
    net.macAddr := PlusArg("macaddr")
    net.rlimit.inc := PlusArg("rlimit-inc", 1)
    net.rlimit.period := PlusArg("rlimit-period", 1)
    net.rlimit.size := PlusArg("rlimit-size", 8)
  }

  def connectSimNetwork(clock: Clock, reset: Bool) {
    val sim = Module(new SimNetwork)
    sim.io.clock := clock
    sim.io.reset := reset
    sim.io.net <> net
  }
}

class NICIOvonly extends Bundle {
  val in = Flipped(Valid(new StreamChannel(NET_IF_WIDTH)))
  val out = Valid(new StreamChannel(NET_IF_WIDTH))
  val macAddr = Input(UInt(ETH_MAC_BITS.W))
  val rlimit = Input(new RateLimiterSettings)

  override def cloneType = (new NICIOvonly).asInstanceOf[this.type]
}

object NICIOvonly {
  def apply(nicio: NICIO): NICIOvonly = {
    val vonly = Wire(new NICIOvonly)
    vonly.out.valid := nicio.out.valid
    vonly.out.bits  := nicio.out.bits
    nicio.out.ready := true.B
    nicio.in.valid  := vonly.in.valid
    nicio.in.bits   := vonly.in.bits
    assert(!vonly.in.valid || nicio.in.ready, "NIC input not ready for valid")
    nicio.macAddr := vonly.macAddr
    nicio.rlimit  := vonly.rlimit
    vonly
  }
}

trait HasPeripheryIceNICModuleImpValidOnly extends LazyModuleImp {
  val outer: HasPeripheryIceNIC
  val net = IO(new NICIOvonly)

  net <> NICIOvonly(outer.icenic.module.io.ext)
}
