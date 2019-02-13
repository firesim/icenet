package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink.TLROM
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest.{UnitTest, UnitTestIO}
import freechips.rocketchip.util._
import testchipip.{StreamIO, StreamChannel, TLHelper}
import scala.math.max
import IceNetConsts._

case class NICConfig(
  inBufPackets: Int = 2,
  outBufFlits: Int = 2 * ETH_MAX_BYTES / NET_IF_BYTES,
  nMemXacts: Int = 8,
  maxAcquireBytes: Int = 64,
  ctrlQueueDepth: Int = 10)

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
  implicit val p: Parameters
  val io: IceNicControllerBundle

  val sendCompDown = Wire(init = false.B)

  val qDepth = p(NICKey).ctrlQueueDepth
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
    extends LazyModule {
  val config = p(NICKey)
  val reader = LazyModule(new StreamReader(
    config.nMemXacts, config.outBufFlits, config.maxAcquireBytes))
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

class IceNicWriter(implicit p: Parameters) extends LazyModule {
  val config = p(NICKey)
  val nXacts = config.nMemXacts
  val maxBytes = config.maxAcquireBytes

  val writer = LazyModule(new StreamWriter(nXacts, maxBytes))
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
    writer.module.io.req.bits.length := io.length << byteAddrBits.U
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
    extends LazyModuleImp(outer) {
  val config = p(NICKey)

  val io = IO(new Bundle {
    val recv = Flipped(new IceNicRecvIO)
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH))) // input stream 
    val tap = outer.tapFuncs.nonEmpty.option(
      Vec(outer.tapFuncs.length, Decoupled(new StreamChannel(NET_IF_WIDTH))))
  })

  val buffer = Module(new NetworkPacketBuffer(config.inBufPackets))
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
    (implicit p: Parameters) extends LazyModule {

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
    val config = p(NICKey)

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

class IceNicTestSendDriver(
    sendReqs: Seq[(Int, Int, Boolean)],
    sendData: Seq[BigInt])(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "test-send-driver", sourceId = IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO {
      val send = new IceNicSendIO
    })

    val (tl, edge) = node.out(0)
    val dataBits = tl.params.dataBits
    val beatBytes = dataBits / 8
    val byteAddrBits = log2Ceil(beatBytes)
    val lenBits = NET_LEN_BITS - 1
    val addrBits = NET_IF_WIDTH - NET_LEN_BITS

    val (s_start :: s_write_req :: s_write_resp ::
         s_send :: s_done :: Nil) = Enum(5)
    val state = RegInit(s_start)

    val sendReqVec = Vec(sendReqs.map {
      case (addr, len, part) => Cat(part.B, len.U(lenBits.W), addr.U(addrBits.W))})

    val sendReqAddrVec = Vec(sendReqs.map{case (addr, _, _) => addr.U(addrBits.W)})
    val sendReqCounts = sendReqs.map { case (_, len, _) => len / beatBytes }
    val sendReqCountVec = Vec(sendReqCounts.map(_.U))
    val sendReqBase = Vec((0 +: (1 until sendReqs.size).map(
      i => sendReqCounts.slice(0, i).reduce(_ + _))).map(_.U(addrBits.W)))
    val maxMemCount = sendReqCounts.reduce(max(_, _))
    val totalMemCount = sendReqCounts.reduce(_ + _)

    require(totalMemCount == sendData.size)

    val sendDataVec = Vec(sendData.map(_.U(dataBits.W)))

    val reqIdx = Reg(UInt(log2Ceil(sendReqs.size).W))
    val memIdx = Reg(UInt(log2Ceil(totalMemCount).W))

    val outSend = TwoWayCounter(
      io.send.req.fire(), io.send.comp.fire(), sendReqs.size)

    val writeAddr = sendReqAddrVec(reqIdx) + (memIdx << byteAddrBits.U)
    val writeData = sendDataVec(sendReqBase(reqIdx) + memIdx)

    tl.a.valid := state === s_write_req
    tl.a.bits := edge.Put(
      fromSource = 0.U,
      toAddress = writeAddr,
      lgSize = byteAddrBits.U,
      data = writeData)._2
    tl.d.ready := state === s_write_resp

    io.send.req.valid := state === s_send
    io.send.req.bits := sendReqVec(reqIdx)
    io.send.comp.ready := outSend =/= 0.U

    io.finished := state === s_done && outSend === 0.U

    when (state === s_start && io.start) {
      reqIdx := 0.U
      memIdx := 0.U
      state := s_write_req
    }

    when (tl.a.fire()) { state := s_write_resp }

    when (tl.d.fire()) {
      memIdx := memIdx + 1.U
      state := s_write_req

      when (memIdx === (sendReqCountVec(reqIdx) - 1.U)) {
        memIdx := 0.U
        reqIdx := reqIdx + 1.U
        when (reqIdx === (sendReqs.size - 1).U) {
          reqIdx := 0.U
          state := s_send
        }
      }
    }

    when (io.send.req.fire()) {
      reqIdx := reqIdx + 1.U
      when (reqIdx === (sendReqs.size - 1).U) {
        reqIdx := 0.U
        state := s_done
      }
    }
  }
}

class IceNicTestRecvDriver(recvReqs: Seq[Int], recvData: Seq[BigInt])
    (implicit p: Parameters) extends LazyModule {

  val node = TLHelper.makeClientNode(
    name = "test-recv-driver", sourceId = IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO {
      val recv = new IceNicRecvIO
    })

    val (tl, edge) = node.out(0)
    val dataBits = tl.params.dataBits
    val beatBytes = dataBits / 8
    val byteAddrBits = log2Ceil(beatBytes)

    val (s_start :: s_recv :: s_wait ::
         s_check_req :: s_check_resp :: s_done :: Nil) = Enum(6)
    val state = RegInit(s_start)

    val recvReqVec = Vec(recvReqs.map(_.U(NET_IF_WIDTH.W)))
    val recvDataVec = Vec(recvData.map(_.U(dataBits.W)))

    val reqIdx = Reg(UInt(log2Ceil(recvReqs.size).W))
    val memIdx = Reg(UInt(log2Ceil(recvData.size).W))

    val outRecv = TwoWayCounter(
      io.recv.req.fire(), io.recv.comp.fire(), recvReqVec.size)

    tl.a.valid := state === s_check_req
    tl.a.bits := edge.Get(
      fromSource = 0.U,
      toAddress = recvReqVec.head + (memIdx << byteAddrBits.U),
      lgSize = byteAddrBits.U)._2
    tl.d.ready := state === s_check_resp

    io.recv.req.valid := state === s_recv
    io.recv.req.bits := recvReqVec(reqIdx)
    io.recv.comp.ready := outRecv =/= 0.U

    io.finished := state === s_done

    when (state === s_start && io.start) {
      reqIdx := 0.U
      memIdx := 0.U
      state := s_recv
    }

    when (io.recv.req.fire()) {
      reqIdx := reqIdx + 1.U
      when (reqIdx === (recvReqVec.size - 1).U) {
        reqIdx := 0.U
        state := s_wait
      }
    }

    when (state === s_wait && outRecv === 0.U) {
      state := s_check_req
    }

    when (state === s_check_req && tl.a.ready) {
      state := s_check_resp
    }

    when (state === s_check_resp && tl.d.valid) {
      memIdx := memIdx + 1.U
      state := s_check_req
      when (memIdx === (recvData.size - 1).U) {
        memIdx := 0.U
        state := s_done
      }
    }

    assert(!tl.d.valid || tl.d.bits.data === recvDataVec(memIdx),
      "IceNicTest: Received wrong data")
  }
}

class IceNicRecvTest(implicit p: Parameters) extends LazyModule {
  val recvReqs = Seq(0, 1440, 1456)
  // The 90-flit packet should be dropped
  val recvLens = Seq(180, 2, 90, 8)
  val testData = Seq.tabulate(280) { i => BigInt(i << 4) }
  val recvData = testData.take(182) ++ testData.drop(272)

  val recvDriver = LazyModule(new IceNicTestRecvDriver(recvReqs, recvData))
  val recvPath = LazyModule(new IceNicRecvPath)
  val xbar = LazyModule(new TLXbar)
  val mem = LazyModule(new TLRAM(
    AddressSet(0, 0x7ff), beatBytes = NET_IF_BYTES))

  val MEM_LATENCY = 32
  val RLIMIT_INC = 1
  val RLIMIT_PERIOD = 4
  val RLIMIT_SIZE = 8

  xbar.node := recvDriver.node
  xbar.node := recvPath.node
  mem.node := TLFragmenter(NET_IF_BYTES, p(NICKey).maxAcquireBytes) :=
    TLHelper.latency(MEM_LATENCY, xbar.node)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    val gen = Module(new PacketGen(recvLens, testData))
    gen.io.start := io.start
    recvDriver.module.io.start := io.start
    recvPath.module.io.recv <> recvDriver.module.io.recv
    recvPath.module.io.in <> RateLimiter(
      gen.io.out, RLIMIT_INC, RLIMIT_PERIOD, RLIMIT_SIZE)
    io.finished := recvDriver.module.io.finished
  }
}

class IceNicRecvTestWrapper(implicit p: Parameters) extends UnitTest(20000) {
  val test = Module(LazyModule(new IceNicRecvTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

class IceNicSendTest(implicit p: Parameters) extends LazyModule {
  val sendReqs = Seq(
    (2, 10, true),
    (17, 6, false),
    (24, 12, false))
  val sendData = Seq(
    BigInt("7766554433221100", 16),
    BigInt("FFEEDDCCBBAA9988", 16),
    BigInt("0123456789ABCDEF", 16),
    BigInt("FEDCBA9876543210", 16),
    BigInt("76543210FDECBA98", 16))

  val recvData = Seq(
    BigInt("9988776655443322", 16),
    BigInt("23456789ABCDBBAA", 16),
    BigInt("FEDCBA9876543210", 16),
    BigInt("00000000FDECBA98", 16))
  val recvKeep = Seq(0xFF, 0xFF, 0xFF, 0x0F)
  val recvLast = Seq(false, true, false, true)

  val sendPath = LazyModule(new IceNicSendPath)
  val rom = LazyModule(new TLROM(0, 64,
    sendData.flatMap(
      data => (0 until 8).map(i => ((data >> (i * 8)) & 0xff).toByte)),
    beatBytes = 8))

  rom.node := TLFragmenter(NET_IF_BYTES, 16) := TLBuffer() := sendPath.node

  val RLIMIT_INC = 1
  val RLIMIT_PERIOD = 0
  val RLIMIT_SIZE = 8

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    val sendPathIO = sendPath.module.io
    val sendReqVec = sendReqs.map { case (start, len, part) =>
      Cat(part.B, len.U(15.W), start.U(48.W))
    }
    val (sendReqIdx, sendReqDone) = Counter(sendPathIO.send.req.fire(), sendReqs.size)
    val (sendCompIdx, sendCompDone) = Counter(sendPathIO.send.comp.fire(), sendReqs.size)

    val started = RegInit(false.B)
    val requesting = RegInit(false.B)
    val completing = RegInit(false.B)

    sendPathIO.send.req.valid := requesting
    sendPathIO.send.req.bits := sendReqVec(sendReqIdx)
    sendPathIO.send.comp.ready := completing

    when (!started && io.start) {
      requesting := true.B
      completing := true.B
    }
    when (sendReqDone)  { requesting := false.B }
    when (sendCompDone) { completing := false.B }

    sendPathIO.rlimit.inc := RLIMIT_INC.U
    sendPathIO.rlimit.period := RLIMIT_PERIOD.U
    sendPathIO.rlimit.size := RLIMIT_SIZE.U

    val check = Module(new PacketCheck(recvData, recvKeep, recvLast))
    check.io.in <> sendPathIO.out
    io.finished := check.io.finished && !completing && !requesting
  }
}

class IceNicSendTestWrapper(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new IceNicSendTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

class IceNicTest(implicit p: Parameters) extends LazyModule {
  val sendReqs = Seq(
    (0, 128, true),
    (144, 160, false),
    (320, 64, false))
  val recvReqs = Seq(256, 544)
  val testData = Seq.tabulate(44)(i => BigInt(i << 4))

  val sendDriver = LazyModule(new IceNicTestSendDriver(sendReqs, testData))
  val recvDriver = LazyModule(new IceNicTestRecvDriver(recvReqs, testData))
  val sendPath = LazyModule(new IceNicSendPath)
  val recvPath = LazyModule(new IceNicRecvPath)
  val xbar = LazyModule(new TLXbar)
  val mem = LazyModule(new TLRAM(
    AddressSet(0, 0x1ff), beatBytes = NET_IF_BYTES))

  val NET_LATENCY = 64
  val MEM_LATENCY = 32
  val RLIMIT_INC = 1
  val RLIMIT_PERIOD = 0
  val RLIMIT_SIZE = 8

  xbar.node := sendDriver.node
  xbar.node := recvDriver.node
  xbar.node := sendPath.node
  xbar.node := recvPath.node
  mem.node := TLFragmenter(NET_IF_BYTES, p(NICKey).maxAcquireBytes) :=
    TLHelper.latency(MEM_LATENCY, xbar.node)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle with UnitTestIO)

    sendPath.module.io.send <> sendDriver.module.io.send
    recvPath.module.io.recv <> recvDriver.module.io.recv

    sendPath.module.io.rlimit.inc := RLIMIT_INC.U
    sendPath.module.io.rlimit.period := RLIMIT_PERIOD.U
    sendPath.module.io.rlimit.size := RLIMIT_SIZE.U

    recvPath.module.io.in <> LatencyPipe(sendPath.module.io.out, NET_LATENCY)

    sendDriver.module.io.start := io.start
    recvDriver.module.io.start := io.start
    io.finished := sendDriver.module.io.finished && recvDriver.module.io.finished

    val count_start :: count_up :: count_print :: count_done :: Nil = Enum(4)
    val count_state = RegInit(count_start)
    val cycle_count = Reg(UInt(64.W))
    val recv_count = Reg(UInt(1.W))

    when (count_state === count_start && sendPath.module.io.send.req.fire()) {
      count_state := count_up
      cycle_count := 0.U
      recv_count := 1.U
    }
    when (count_state === count_up) {
      cycle_count := cycle_count + 1.U
      when (recvPath.module.io.recv.comp.fire()) {
        recv_count := recv_count - 1.U
        when (recv_count === 0.U) { count_state := count_print }
      }
    }
    when (count_state === count_print) {
      printf("NIC test completed in %d cycles\n", cycle_count)
      count_state := count_done
    }
  }
}

class IceNicTestWrapper(implicit p: Parameters) extends UnitTest(50000) {
  val test = Module(LazyModule(new IceNicTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}
