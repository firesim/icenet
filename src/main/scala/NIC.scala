package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.coreplex.HasSystemBus
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{TwoWayCounter, PlusArg}
import testchipip.{StreamIO, StreamChannel, SeqQueue, TLHelper}
import scala.util.Random
import IceNetConsts._

case class NICConfig(
  inBufPackets: Int = 2,
  outBufFlits: Int = 2 * ETH_MAX_BYTES / NET_IF_BYTES,
  nMemXacts: Int = 8,
  maxAcquireBytes: Int = 64)

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

trait IceNicControllerModule extends HasRegMap {
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

class IceNicSendPath(implicit p: Parameters) extends LazyModule {
  val reader = LazyModule(new IceNicReader)
  val node = TLIdentityNode()
  node := reader.node

  val config = p(NICKey)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val send = Flipped(new IceNicSendIO)
      val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
      val rlimit = Input(new RateLimiterSettings)
    })

    reader.module.io.send <> io.send

    val limiter = Module(new RateLimiter(new StreamChannel(NET_IF_WIDTH)))
    limiter.io.in <> SeqQueue(reader.module.io.out, config.outBufFlits)
    limiter.io.settings := io.rlimit
    io.out <> limiter.io.out
  }
}

/*
 * Send frames out
 */
class IceNicReader(implicit p: Parameters)
    extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "ice-nic-send", sourceId = IdRange(0, 1))
  lazy val module = new IceNicReaderModule(this)
}

class IceNicReaderModule(outer: IceNicReader)
    extends LazyModuleImp(outer) {

  val io = IO(new Bundle {
    val send = Flipped(new IceNicSendIO)
    val out = Decoupled(new StreamChannel(NET_IF_WIDTH))
  })

  val (tl, edge) = outer.node.out(0)
  val beatBytes = tl.params.dataBits / 8
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = tl.params.addressBits
  val lenBits = NET_LEN_BITS - 1
  val midPoint = NET_IF_WIDTH - NET_LEN_BITS
  val packpart = io.send.req.bits(NET_IF_WIDTH - 1)
  val packlen = io.send.req.bits(NET_IF_WIDTH - 2, midPoint)
  val packaddr = io.send.req.bits(midPoint - 1, 0)
  val maxBytes = p(NICKey).maxAcquireBytes

  // we allow one TL request at a time to avoid tracking
  val s_idle :: s_read :: s_send :: s_comp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // Physical (word) address in memory
  val sendaddr = Reg(UInt(addrBits.W))
  // Number of words to send
  val sendlen  = Reg(UInt(lenBits.W))
  // 0 if last packet in sequence, 1 otherwise
  val sendpart = Reg(Bool())

  val grantqueue = Queue(tl.d, 1)

  val reqSize = MuxCase(byteAddrBits.U,
    (log2Ceil(maxBytes) until byteAddrBits by -1).map(lgSize =>
        // Use the largest size (beatBytes <= size <= maxBytes)
        // s.t. sendaddr % size == 0 and sendlen > size
        (sendaddr(lgSize-1,0) === 0.U &&
          (sendlen >> lgSize.U) =/= 0.U) -> lgSize.U))
  val beatsLeft = Reg(UInt(log2Ceil(maxBytes / beatBytes).W))

  io.send.req.ready := state === s_idle
  tl.a.valid := state === s_read
  tl.a.bits := edge.Get(
    fromSource = 0.U,
    toAddress = sendaddr,
    lgSize = reqSize)._2
  io.out.valid := grantqueue.valid && state === s_send
  io.out.bits.data := grantqueue.bits.data
  io.out.bits.keep := ~0.U(beatBytes.W)
  io.out.bits.last := sendlen === 0.U && beatsLeft === 0.U && !sendpart
  grantqueue.ready := io.out.ready && state === s_send
  io.send.comp.valid := state === s_comp
  io.send.comp.bits := true.B

  when (io.send.req.fire()) {
    sendaddr := packaddr
    sendlen  := packlen
    sendpart := packpart
    state := s_read

    assert(packaddr(byteAddrBits-1,0) === 0.U &&
           packlen(byteAddrBits-1,0)  === 0.U,
           s"NIC send address and length must be aligned to ${beatBytes} bytes")
  }

  when (tl.a.fire()) {
    val reqBytes = 1.U << reqSize
    sendaddr := sendaddr + reqBytes
    sendlen  := sendlen - reqBytes
    beatsLeft := (reqBytes >> byteAddrBits.U) - 1.U
    state := s_send
  }

  when (io.out.fire()) {
    when (beatsLeft === 0.U) {
      state := Mux(sendlen === 0.U, s_comp, s_read)
    } .otherwise { beatsLeft := beatsLeft - 1.U }
  }

  when (io.send.comp.fire()) {
    state := s_idle
  }
}

class IceNicWriter(implicit p: Parameters) extends LazyModule {
  val nXacts = p(NICKey).nMemXacts
  val node = TLHelper.makeClientNode(
    name = "ice-nic-recv", sourceId = IdRange(0, nXacts))
  lazy val module = new IceNicWriterModule(this)
}

class IceNicWriterModule(outer: IceNicWriter)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val length = Input(UInt(NET_LEN_BITS.W))
  })

  val maxBytes = p(NICKey).maxAcquireBytes
  val (tl, edge) = outer.node.out(0)
  val beatBytes = tl.params.dataBits / 8
  val fullAddrBits = tl.params.addressBits
  val byteAddrBits = log2Ceil(beatBytes)
  val addrBits = fullAddrBits - byteAddrBits

  val s_idle :: s_data :: s_complete :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val baseAddr = Reg(UInt(addrBits.W))
  val idx = Reg(UInt(addrBits.W))
  val addrMerged = baseAddr + idx

  val xactBusy = RegInit(0.U(outer.nXacts.W))
  val xactOnehot = PriorityEncoderOH(~xactBusy)

  val maxBeats = maxBytes / beatBytes
  val beatIdBits = log2Ceil(maxBeats)

  val beatsLeft = Reg(UInt(beatIdBits.W))
  val headAddr = Reg(UInt(addrBits.W))
  val headXact = Reg(UInt(log2Ceil(outer.nXacts).W))
  val headSize = Reg(UInt(log2Ceil(beatIdBits + 1).W))

  val newBlock = beatsLeft === 0.U
  val canSend = !xactBusy.andR || !newBlock

  val reqSize = MuxCase(0.U,
    (log2Ceil(maxBytes / beatBytes) until 0 by -1).map(lgSize =>
        (addrMerged(lgSize-1,0) === 0.U &&
          (io.length >> lgSize.U) =/= 0.U) -> lgSize.U))

  xactBusy := (xactBusy | Mux(tl.a.fire() && newBlock, xactOnehot, 0.U)) &
                  ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

  val fromSource = Mux(newBlock, OHToUInt(xactOnehot), headXact)
  val toAddress = Mux(newBlock, addrMerged, headAddr) << byteAddrBits.U
  val lgSize = Mux(newBlock, reqSize, headSize) +& byteAddrBits.U

  io.recv.req.ready := state === s_idle
  tl.a.valid := (state === s_data && io.in.valid) && canSend
  tl.a.bits := edge.Put(
    fromSource = fromSource,
    toAddress = toAddress,
    lgSize = lgSize,
    data = io.in.bits.data)._2
  tl.d.ready := xactBusy.orR
  io.in.ready := state === s_data && canSend && tl.a.ready
  io.recv.comp.valid := state === s_complete && !xactBusy.orR
  io.recv.comp.bits := idx << byteAddrBits.U

  when (io.recv.req.fire()) {
    idx := 0.U
    baseAddr := io.recv.req.bits >> byteAddrBits.U
    beatsLeft := 0.U
    state := s_data
  }

  when (tl.a.fire()) {
    when (newBlock) {
      val bytesToWrite = 1.U << reqSize
      beatsLeft := bytesToWrite - 1.U
      headAddr := addrMerged
      headXact := OHToUInt(xactOnehot)
      headSize := reqSize
    } .otherwise {
      beatsLeft := beatsLeft - 1.U
    }
    idx := idx + 1.U
    when (io.in.bits.last) { state := s_complete }
  }

  when (io.recv.comp.fire()) { state := s_idle }
}

/*
 * Recv frames
 */
class IceNicRecvPath(implicit p: Parameters) extends LazyModule {
  val writer = LazyModule(new IceNicWriter)
  val node = TLIdentityNode()
  node := writer.node
  lazy val module = new IceNicRecvPathModule(this)
}

class IceNicRecvPathModule(outer: IceNicRecvPath)
    extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH))) // input stream 
  })

  val config = p(NICKey)
  val buffer = Module(new NetworkPacketBuffer(config.inBufPackets))
  buffer.io.stream.in <> io.in

  val writer = outer.writer.module
  writer.io.in <> buffer.io.stream.out
  writer.io.length := buffer.io.length
  writer.io.recv <> io.recv
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
class IceNIC(address: BigInt, beatBytes: Int = 8)
    (implicit p: Parameters) extends LazyModule {

  val control = LazyModule(new IceNicController(
    IceNicControllerParams(address, beatBytes)))
  val sendPath = LazyModule(new IceNicSendPath)
  val recvPath = LazyModule(new IceNicRecvPath)

  val mmionode = TLIdentityNode()
  val dmanode = TLIdentityNode()
  val intnode = IntIdentityNode()

  control.node := mmionode
  dmanode := sendPath.node
  dmanode := recvPath.node
  intnode := control.intnode

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ext = new NICIO
    })

    val config = p(NICKey)

    sendPath.module.io.send <> control.module.io.send
    recvPath.module.io.recv <> control.module.io.recv

    // connect externally
    recvPath.module.io.in <> io.ext.in
    io.ext.out <> sendPath.module.io.out

    control.module.io.macAddr := io.ext.macAddr
    sendPath.module.io.rlimit := io.ext.rlimit
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

  val icenic = LazyModule(new IceNIC(address, sbus.beatBytes))
  icenic.mmionode := sbus.toVariableWidthSlaves
  sbus.fromSyncPorts() :=* icenic.dmanode
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

