package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest.{HasUnitTestIO, UnitTest}
import freechips.rocketchip.util.HellaPeekingArbiter
import testchipip._
import IceNetConsts._

class DistributionBuffer[T <: Data](typ: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Vec(n, Decoupled(typ))
    val route = Input(UInt(n.W))
  })

  val tosend = RegInit(0.U(n.W))
  val senddata = Reg(typ)

  io.out.zip(tosend.toBools).map { case (out, send) =>
    out.valid := send
    out.bits := senddata
  }

  val next_tosend = (tosend & ~Cat(io.out.map(_.fire()).reverse))
  tosend := next_tosend
  io.in.ready := !next_tosend.orR

  when (io.in.fire()) {
    tosend := io.route
    senddata := io.in.bits
  }
}

class SimpleSwitchRouter(id: Int, n: Int) extends Module {
  val io = IO(new Bundle {
    val header = Flipped(Valid(new EthernetHeader))
    val in = Flipped(Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val out = Vec(n, Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val tcam = new TCAMMatchIO(n, ETH_MAC_BITS)
  })

  val s_idle :: s_tcam :: s_forward :: s_drop :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val dstmac = Reg(UInt(ETH_MAC_BITS.W))
  val route = Reg(UInt(n.W))

  val buffer = Module(new DistributionBuffer(
    new StreamChannel(NET_IF_WIDTH), n))

  io.tcam.data := dstmac
  io.out <> buffer.io.out
  io.in.ready := MuxLookup(state, false.B, Seq(
    s_forward -> buffer.io.in.ready,
    s_drop -> true.B))
  buffer.io.in.valid := state === s_forward && io.in.valid
  buffer.io.in.bits := io.in.bits
  buffer.io.route := route

  when (state === s_idle && io.header.valid) {
    dstmac := io.header.bits.dstmac
    state := s_tcam
  }
  when (state === s_tcam) {
    when (dstmac.andR) {
      route := ~0.U(n.W) ^ (1 << id).U
      state := s_forward
    } .otherwise {
      route := UIntToOH(io.tcam.addr)
      state := Mux(io.tcam.found, s_forward, s_drop)
    }
  }
  when (io.in.fire() && io.in.bits.last) {
    state := s_idle
  }
}

class SimpleSwitchCrossbar(n: Int) extends Module {
  val io = IO(new Bundle {
    val headers = Flipped(Vec(n, Valid(new EthernetHeader)))
    val in = Flipped(Vec(n, Decoupled(new StreamChannel(NET_IF_WIDTH))))
    val out = Vec(n, Decoupled(new StreamChannel(NET_IF_WIDTH)))
    val tcam = Vec(n, new TCAMMatchIO(n, ETH_MAC_BITS))
  })

  val routers = Seq.tabulate(n) { i => Module(new SimpleSwitchRouter(i, n)) }

  for (i <- 0 until n) {
    val r = routers(i)
    r.io.header := io.headers(i)
    r.io.in <> io.in(i)
    io.tcam(i) <> r.io.tcam
  }

  for (i <- 0 until n) {
    val arb = Module(new HellaPeekingArbiter(
      new StreamChannel(NET_IF_WIDTH), n,
      (ch: StreamChannel) => ch.last, rr = true))
    arb.io.in <> routers.map(r => r.io.out(i))
    io.out(i) <> arb.io.out
  }
}

class SimpleSwitch(address: BigInt, n: Int)
    (implicit p: Parameters) extends LazyModule {

  val node = TLInputNode()
  val tcam = LazyModule(new TCAM(address, n, NET_IF_WIDTH, n))

  tcam.node := node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val tl = node.bundleIn
      val streams = Vec(n, new StreamIO(NET_IF_WIDTH))
    })
    val inBuffers  = Seq.fill(n) { Module(new NetworkPacketBuffer(2)) }
    val outBuffers = Seq.fill(n) { Module(new NetworkPacketBuffer(2)) }
    val xbar = Module(new SimpleSwitchCrossbar(n))

    for (i <- 0 until n) {
      val inBuf = inBuffers(i)
      val outBuf = outBuffers(i)
      val stream = io.streams(i)

      inBuf.io.stream.in <> stream.in
      xbar.io.in(i) <> inBuf.io.stream.out
      xbar.io.headers(i) <> inBuf.io.header
      outBuf.io.stream.in <> xbar.io.out(i)
      stream.out <> outBuf.io.stream.out
      tcam.module.io.tcam(i) <> xbar.io.tcam(i)
    }
  }
}

class SwitchTestSetup(macaddrs: Seq[Long])
    (implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    name = "switch-test-setup",
    sourceId = IdRange(0, 1)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val tl = node.bundleOut
      val start = Input(Bool())
      val finished = Output(Bool())
    })

    val s_idle :: s_acq :: s_gnt :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val tcamData = Vec(
      macaddrs.map(_.U(ETH_MAC_BITS.W)) ++
      Seq.fill(macaddrs.size) { ~0.U(ETH_MAC_BITS.W) })

    val tl = io.tl(0)
    val edge = node.edgesOut(0)

    val (writeCnt, writeDone) = Counter(tl.d.fire(), macaddrs.size * 2)

    tl.a.valid := state === s_acq
    tl.a.bits := edge.Put(
      fromSource = 0.U,
      lgSize = 3.U,
      toAddress = writeCnt << 3.U,
      data = tcamData(writeCnt))._2
    tl.d.ready := state === s_gnt
    io.finished := state === s_done

    when (state === s_idle && io.start) { state := s_acq }
    when (tl.a.fire()) { state := s_gnt }
    when (tl.d.fire()) { state := s_acq }
    when (writeDone) { state := s_done }
  }
}



class BasicSwitchTestClient(srcmac: Long, dstmac: Long) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val finished = Output(Bool())
    val net = new StreamIO(NET_IF_WIDTH)
  })

  val sendHeader = Wire(new EthernetHeader)
  sendHeader.srcmac := srcmac.U
  sendHeader.dstmac := dstmac.U
  sendHeader.ethType := 0.U
  sendHeader.padding := 0.U

  val headerWords = 128 / NET_IF_WIDTH
  val sendPacket = Vec(
    sendHeader.toWords() ++ Seq(1, 2, 3, 4).map(_.U(64.W)))
  val recvPacket = Reg(Vec(sendPacket.size, UInt(NET_IF_WIDTH.W)))
  val recvHeader = (new EthernetHeader).fromWords(recvPacket)
  val headerDone = RegInit(false.B)

  val (outCnt, outDone) = Counter(io.net.out.fire(), sendPacket.size)
  val (inCnt, inDone) = Counter(io.net.in.fire(), recvPacket.size)

  val s_idle :: s_send :: s_recv :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  when (state === s_idle && io.start) {
    state := s_send
  }

  when (outDone) { state := s_recv }
  when (io.net.in.fire()) {
    recvPacket(inCnt) := io.net.in.bits.data
    when (inCnt === (headerWords-1).U) { headerDone := true.B }
    when (io.net.in.bits.last) { state := s_done }
  }

  assert(!headerDone ||
    (recvHeader.srcmac === sendHeader.dstmac &&
     recvHeader.dstmac === sendHeader.srcmac),
   "BasicSwitchTest: Returned header incorrect")

  assert(!headerDone || !io.net.in.valid ||
    io.net.in.bits.data === sendPacket(inCnt),
    "BasicSwitchTest: Returned payload incorrect")

  io.finished := state === s_done
  io.net.out.valid := state === s_send
  io.net.out.bits.data := sendPacket(outCnt)
  io.net.out.bits.last := outCnt === (sendPacket.size-1).U
  io.net.in.ready := state === s_recv
}

class BasicSwitchTestServer extends Module {
  val io = IO(new Bundle {
    val net = new StreamIO(NET_IF_WIDTH)
  })

  val headerWords = 128 / NET_IF_WIDTH
  val recvPacket = Reg(Vec(headerWords + 4, UInt(NET_IF_WIDTH.W)))
  val recvHeader = (new EthernetHeader).fromWords(recvPacket)

  val sendHeader = Wire(new EthernetHeader)
  sendHeader.ethType := 0.U
  sendHeader.padding := 0.U
  sendHeader.dstmac := recvHeader.srcmac
  sendHeader.srcmac := recvHeader.dstmac

  val sendPacket = Vec(sendHeader.toWords() ++ recvPacket.drop(headerWords))
  val sending = RegInit(false.B)

  val (recvCnt, recvDone) = Counter(io.net.in.fire(), recvPacket.size)
  val (sendCnt, sendDone) = Counter(io.net.out.fire(), sendPacket.size)

  io.net.in.ready := !sending
  io.net.out.valid := sending
  io.net.out.bits.data := sendPacket(sendCnt)
  io.net.out.bits.last := sendCnt === (sendPacket.size-1).U

  when (io.net.in.fire()) { recvPacket(recvCnt) := io.net.in.bits.data }
  when (recvDone) { sending := true.B }
  when (sendDone) { sending := false.B }
}

class BasicSwitchTest(implicit p: Parameters) extends LazyModule {
  val clientMac = 0x665544332211L
  val serverMac = 0xCCBBAA998877L

  val switch = LazyModule(new SimpleSwitch(BigInt(0), 2))
  val setup = LazyModule(new SwitchTestSetup(Seq(clientMac, serverMac)))

  switch.node := setup.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val client = Module(new BasicSwitchTestClient(clientMac, serverMac))
    val server = Module(new BasicSwitchTestServer)

    switch.module.io.streams.zip(Seq(client.io.net, server.io.net)).foreach {
      case (a, b) => a.flipConnect(b)
    }

    setup.module.io.start := io.start
    client.io.start := setup.module.io.finished
    io.finished := client.io.finished
  }
}

class BasicSwitchTestWrapper(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new BasicSwitchTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

class BroadcastTestSender(mac: Long) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val net = new StreamIO(NET_IF_WIDTH)
  })

  val s_idle :: s_send :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val sendHeader = Wire(new EthernetHeader)
  sendHeader.padding := 0.U
  sendHeader.ethType := 0.U
  sendHeader.dstmac := 0xFFFFFFFFFFFFL.U
  sendHeader.srcmac := mac.U

  val sendPacket = Vec(sendHeader.toWords() ++ Seq(1, 2, 3, 4).map(_.U(64.W)))
  val (sendCnt, sendDone) = Counter(io.net.out.fire(), sendPacket.size)

  io.net.out.valid := state === s_send
  io.net.out.bits.data := sendPacket(sendCnt)
  io.net.out.bits.last := sendCnt === (sendPacket.size - 1).U
  io.net.in.ready := false.B

  when (state === s_idle && io.start) {
    state := s_send
  }

  when (sendDone) { state := s_done }
}

class BroadcastTestReceiver(srcmac: Long) extends Module {
  val io = IO(new Bundle {
    val net = new StreamIO(NET_IF_WIDTH)
    val finished = Output(Bool())
  })

  val s_recv :: s_done :: Nil = Enum(2)
  val state = RegInit(s_recv)

  io.net.out.valid := false.B
  io.net.in.ready := state === s_recv
  io.finished := state === s_done

  val expectedHeader = Wire(new EthernetHeader)
  expectedHeader.padding := 0.U
  expectedHeader.ethType := 0.U
  expectedHeader.dstmac := 0xFFFFFFFFFFFFL.U
  expectedHeader.srcmac := srcmac.U

  val expectedPacket = Vec(
    expectedHeader.toWords() ++ Seq(1, 2, 3, 4).map(_.U(64.W)))
  val (recvCnt, recvDone) = Counter(
    io.net.in.fire(), expectedPacket.size)

  when (recvDone) { state := s_done }

  assert(!io.net.in.valid || io.net.in.bits.data === expectedPacket(recvCnt),
    "BroadcastTest: incorrect data received")

  assert(!recvDone || io.net.in.bits.last,
    "BroadcastTest: Last not asserted for final word")
}

class BroadcastTest(implicit p: Parameters) extends LazyModule {
  val macAddrs = Seq(
    0x554433221100L, 0xBBAA99887766L,
    0x1100FFEEDDCCL, 0x776655443322L)
  val setup = LazyModule(new SwitchTestSetup(macAddrs))
  val switch = LazyModule(new SimpleSwitch(0, 4))
  
  switch.node := setup.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val sender = Module(new BroadcastTestSender(macAddrs.head))
    val receivers = Seq.fill(3) {
      Module(new BroadcastTestReceiver(macAddrs.head))
    }
    val allNets = sender.io.net +: receivers.map(_.io.net)

    switch.module.io.streams.zip(allNets).foreach {
      case (a, b) => a.flipConnect(b)
    }

    sender.io.start := io.start
    io.finished := receivers.map(_.io.finished).reduce(_ && _)
  }
}

class BroadcastTestWrapper(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new BroadcastTest).module)
  test.io.start := io.start
  io.finished := test.io.finished
}
