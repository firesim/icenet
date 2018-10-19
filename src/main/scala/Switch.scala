package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.unittest.{HasUnitTestIO, UnitTest, UnitTestIO}
import freechips.rocketchip.util.HellaPeekingArbiter
import testchipip._
import IceNetConsts._

/**
 * Reroutes a input type to n locations depending on the route input
 *
 * @param typ type of item that the buffer stores
 * @param n number of places to route the input signal
 */
class DistributionBuffer[T <: Data](typ: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Vec(n, Decoupled(typ))
    val route = Input(UInt(n.W))
  })

  val tosend = RegInit(0.U(n.W))
  val senddata = Reg(typ)

  // map all the send signals to the output and connect the single input type to the senddata output
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

/** 
 * Description of SimpleSwitchRouter
 *
 * @param id id of the router
 * @param n size of the router
 * @param ifWidthBits flit size that the router can handle
 */
class SimpleSwitchRouter(id: Int, n: Int, ifWidthBits: Int) extends Module {
  val io = IO(new Bundle {
    val header = Flipped(Valid(new EthernetHeader))
    val in = Flipped(Decoupled(new StreamChannel(ifWidthBits)))
    val out = Vec(n, Decoupled(new StreamChannel(ifWidthBits)))
    val tcam = new TCAMMatchIO(n, ETH_MAC_BITS)
  })

  val s_idle :: s_tcam :: s_forward :: s_drop :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val dstmac = Reg(UInt(ETH_MAC_BITS.W))
  val route = Reg(UInt(n.W))

  val buffer = Module(new DistributionBuffer(new StreamChannel(ifWidthBits), n))

  io.tcam.data := dstmac
  io.out <> buffer.io.out
  io.in.ready := MuxLookup(state, false.B, Seq(
    s_forward -> buffer.io.in.ready,
    s_drop -> true.B))
  buffer.io.in.valid := state === s_forward && io.in.valid
  buffer.io.in.bits := io.in.bits
  buffer.io.route := route

  when (state === s_idle && io.header.valid) {
    //AJG: TODO: Remove
    //printf("dstmac := 0x%x\n", io.header.bits.dstmac)
    dstmac := io.header.bits.dstmac
    state := s_tcam
  }
  when (state === s_tcam) {
    when (dstmac.andR) {
      //AJG: TODO: Remove
      //printf("Forward the packet with route: 0x%x\n", ~0.U(n.W) ^ (1 << id).U)
      route := ~0.U(n.W) ^ (1 << id).U
      state := s_forward
    }
    .otherwise {
      //AJG: TODO: Remove
      //when(io.tcam.found){
      //  printf("Forward the packet with route: 0x%x orig addr 0x%x\n", UIntToOH(io.tcam.addr), io.tcam.addr)
      //}
      //.otherwise{
      //  printf("Drop the packet\n")
      //}
      route := UIntToOH(io.tcam.addr)
      state := Mux(io.tcam.found, s_forward, s_drop)
    }
  }
  when (io.in.fire() && io.in.bits.last) {
    state := s_idle
  }
}

/**
 * Description of SimpleSwitchCrossbar
 *
 * @param n size of the crossbar
 * @param ifWidthBits flit size that the crossbar can handle
 */
class SimpleSwitchCrossbar(n: Int, ifWidthBits: Int) extends Module {
  val io = IO(new Bundle {
    val headers = Flipped(Vec(n, Valid(new EthernetHeader)))
    val in = Flipped(Vec(n, Decoupled(new StreamChannel(ifWidthBits))))
    val out = Vec(n, Decoupled(new StreamChannel(ifWidthBits)))
    val tcam = Vec(n, new TCAMMatchIO(n, ETH_MAC_BITS))
  })

  val routers = Seq.tabulate(n) { i => Module(new SimpleSwitchRouter(i, n, ifWidthBits)) }

  for (i <- 0 until n) {
    val r = routers(i)
    r.io.header := io.headers(i)
    r.io.in <> io.in(i)
    io.tcam(i) <> r.io.tcam
  }

  for (i <- 0 until n) {
    val arb = Module(new HellaPeekingArbiter(
      new StreamChannel(ifWidthBits), n,
      (ch: StreamChannel) => ch.last, rr = true))
    arb.io.in <> routers.map(r => r.io.out(i))
    io.out(i) <> arb.io.out
  }
}

/** 
 * Simple switch
 *
 * @param address address used to tell where the switch is located
 * @param n size of the switch
 * @param netConfig configuration parameters for network
 */
class SimpleSwitch(address: BigInt, n: Int, netConfig: IceNetConfig)
    (implicit p: Parameters) extends LazyModule {

  val node = TLIdentityNode()

  // AJG: TODO: Ask Howie if it is fine to make this 8B large. MAC addr's will probably not go larger than
  // 8B. At minimum this needs to be ETH_MAC_SIZE
  val tcam = LazyModule(new TCAM(address, n, 64, n))

  tcam.node := node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val streams = Vec(n, new StreamIO(netConfig.NET_IF_WIDTH_BITS))
    })

    val inBuffers  = Seq.fill(n) { Module(new NetworkPacketBuffer(nPackets = 2, headerType = new EthernetHeader, wordBytes = netConfig.NET_IF_WIDTH_BYTES)) }
    val outBuffers = Seq.fill(n) {
      val ethWords = if(netConfig.NET_IF_WIDTH_BITS > ETH_MAX_BYTES * 8) 1 else (ETH_MAX_BYTES * 8)/netConfig.NET_IF_WIDTH_BITS
      Module(new Queue(new StreamChannel(netConfig.NET_IF_WIDTH_BITS), ethWords))
    }
    val xbar = Module(new SimpleSwitchCrossbar(n, netConfig.NET_IF_WIDTH_BITS))

    for (i <- 0 until n) {
      val inBuf = inBuffers(i)
      val outBuf = outBuffers(i)
      val stream = io.streams(i)

      inBuf.io.stream.in <> stream.in
      xbar.io.in(i) <> inBuf.io.stream.out
      xbar.io.headers(i) <> inBuf.io.header
      outBuf.io.enq <> xbar.io.out(i)
      stream.out <> outBuf.io.deq
      tcam.module.io.tcam(i) <> xbar.io.tcam(i)
    }
  }
}

class SwitchTestSetup(macaddrs: Seq[Long])
    (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "switch-test-setup",
    sourceId = IdRange(0, 1))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val start = Input(Bool())
      val finished = Output(Bool())
    })

    val s_idle :: s_acq :: s_gnt :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val tcamData = VecInit(
      macaddrs.map(_.U(ETH_MAC_BITS.W)) ++
      Seq.fill(macaddrs.size) { ~0.U(ETH_MAC_BITS.W) })

    val (tl, edge) = node.out(0)

    val (writeCnt, writeDone) = Counter(tl.d.fire(), macaddrs.size * 2)

    tl.a.valid := state === s_acq
    tl.a.bits := edge.Put(
      fromSource = 0.U,
      lgSize = 3.U, //AJG: TODO: This should correspond with the decision of the size of the TCAM. If they don't line up then the mask breaks things
      toAddress = Cat(0.U(1.W), writeCnt << 3.U),
      data = tcamData(writeCnt))._2
    tl.d.ready := state === s_gnt
    io.finished := state === s_done

    when (state === s_idle && io.start) { state := s_acq }
    when (tl.a.fire()) { 
      state := s_gnt
      //AJG: TODO: Remove
      //printf("TCAM Data(%d) := (0x%x)\n", writeCnt, tcamData(writeCnt))
    }
    when (tl.d.fire()) { state := s_acq }
    when (writeDone) { state := s_done }
  }
}



class BasicSwitchTestClient(srcmac: Long, dstmac: Long, netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val finished = Output(Bool())
    val net = new StreamIO(netConfig.NET_IF_WIDTH_BITS)
  })

  val sendHeader = EthernetHeader(dstmac.U, srcmac.U, 0.U)
  val sendPacket = VecInit(sendHeader.toWords(netConfig.NET_IF_WIDTH_BITS) ++ Seq(1, 2, 3, 4).map(_.U(netConfig.NET_IF_WIDTH_BITS.W)))
  val recvPacket = Reg(Vec(sendPacket.size, UInt(netConfig.NET_IF_WIDTH_BITS.W)))
  val recvHeader = (new EthernetHeader).fromWords(recvPacket, netConfig.NET_IF_WIDTH_BITS)
  val headerDone = RegInit(false.B)

  val (outCnt, outDone) = Counter(io.net.out.fire(), sendPacket.size)
  val (inCnt, inDone) = Counter(io.net.in.fire(), recvPacket.size)

  val s_idle :: s_send :: s_recv :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  val headerWords = if(netConfig.NET_IF_WIDTH_BITS > ETH_HEAD_BYTES * 8) 1 else (ETH_HEAD_BYTES * 8)/netConfig.NET_IF_WIDTH_BITS

  when (state === s_idle && io.start) {
    state := s_send
  }

  //AJG: TODO: Remove
  //printf("---Client---\n")
  //when(io.net.in.fire()){
  //  printf("Recieved: recvPacket(%d) := 0x%x\n", inCnt, io.net.in.bits.data)
  //}
  //when(io.net.out.fire()){
  //  printf("Sending: sendPacket(%d) := 0x%x\n", outCnt, io.net.out.bits.data) 
  //}


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

  //AJG: TODO: Remove
  //when(!(!headerDone ||
  //  (recvHeader.srcmac === sendHeader.dstmac &&
  //   recvHeader.dstmac === sendHeader.srcmac))){
  //     printf("Incorrect header\n")
  //     printf("Wrong: recvHeader: total(0x%x) srcmac(0x%x) destmac(0x%x)\n", Cat(recvPacket.take(headerWords).reverse), recvHeader.srcmac, recvHeader.dstmac)
  //     printf("Wrong: sendHeader: total(0x%x) srcmac(0x%x) destmac(0x%x)\n", sendHeader.asTypeOf(UInt()), sendHeader.srcmac, sendHeader.dstmac)
  //   }
  //printf("---Client Done---\n")

  io.finished := state === s_done
  io.net.out.valid := state === s_send
  io.net.out.bits.keep := netConfig.NET_FULL_KEEP 
  io.net.out.bits.data := sendPacket(outCnt)
  io.net.out.bits.last := outCnt === (sendPacket.size-1).U
  io.net.in.ready := state === s_recv
}

class BasicSwitchTestServer(netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle {
    val net = new StreamIO(netConfig.NET_IF_WIDTH_BITS)
  })

  val headerWords = if(netConfig.NET_IF_WIDTH_BITS > ETH_HEAD_BYTES * 8) 1 else (ETH_HEAD_BYTES * 8)/netConfig.NET_IF_WIDTH_BITS
  val recvPacket = Reg(Vec(headerWords + 4, UInt(netConfig.NET_IF_WIDTH_BITS.W)))
  val recvHeader = (new EthernetHeader).fromWords(recvPacket, netConfig.NET_IF_WIDTH_BITS)

  val sendHeader = EthernetHeader(recvHeader.srcmac, recvHeader.dstmac, 0.U)
  val sendPacket = VecInit(sendHeader.toWords(netConfig.NET_IF_WIDTH_BITS) ++ recvPacket.drop(headerWords))
  val sending = RegInit(false.B)

  val (recvCnt, recvDone) = Counter(io.net.in.fire(), recvPacket.size)
  val (sendCnt, sendDone) = Counter(io.net.out.fire(), sendPacket.size)

  io.net.in.ready := !sending
  io.net.out.valid := sending
  io.net.out.bits.keep := netConfig.NET_FULL_KEEP 
  io.net.out.bits.data := sendPacket(sendCnt)
  io.net.out.bits.last := sendCnt === (sendPacket.size-1).U

  //printf("---Server---\n")
  //when(io.net.in.fire()){
  //  printf("Recieved: recvPacket(%d) := 0x%x\n", recvCnt, io.net.in.bits.data)
  //}
  //when(io.net.out.fire()){
  //  printf("Sending: sendPacket(%d) := 0x%x\n", sendCnt, io.net.out.bits.data) 
  //}
  //printf("---Server Done---\n")

  when (io.net.in.fire()) { recvPacket(recvCnt) := io.net.in.bits.data }
  when (recvDone) { sending := true.B }
  when (sendDone) { sending := false.B }
}

class BasicSwitchTest(testWidth: Int = 64)(implicit p: Parameters) extends LazyModule {
  val clientMac = 0x665544332211L
  val serverMac = 0xCCBBAA998877L

  val netConfig = new IceNetConfig(NET_IF_WIDTH_BITS = testWidth)
  val switch = LazyModule(new SimpleSwitch(BigInt(0), 2, netConfig))
  val setup = LazyModule(new SwitchTestSetup(Seq(clientMac, serverMac)))

  switch.node := setup.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val io = IO(new Bundle with UnitTestIO)
    val client = Module(new BasicSwitchTestClient(clientMac, serverMac, netConfig))
    val server = Module(new BasicSwitchTestServer(netConfig))

    switch.module.io.streams.zip(Seq(client.io.net, server.io.net)).foreach {
      case (a, b) => a.flipConnect(b)
    }

    setup.module.io.start := io.start
    client.io.start := setup.module.io.finished
    io.finished := client.io.finished
  }
}

class BasicSwitchTestWrapper(testWidth: Int = 64)(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new BasicSwitchTest(testWidth)).module)
  test.io.start := io.start
  io.finished := test.io.finished
}

class BroadcastTestSender(mac: Long, netConfig: IceNetConfig) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val net = new StreamIO(netConfig.NET_IF_WIDTH_BITS)
  })

  val s_idle :: s_send :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val sendHeader = EthernetHeader(ETH_BCAST_MAC, mac.U, 0.U)
  val sendPacket = VecInit(sendHeader.toWords(netConfig.NET_IF_WIDTH_BITS) ++ Seq(1, 2, 3, 4).map(_.U(netConfig.NET_IF_WIDTH_BITS.W)))
  val (sendCnt, sendDone) = Counter(io.net.out.fire(), sendPacket.size)

  io.net.out.valid := state === s_send
  io.net.out.bits.keep := netConfig.NET_FULL_KEEP
  io.net.out.bits.data := sendPacket(sendCnt)
  io.net.out.bits.last := sendCnt === (sendPacket.size - 1).U
  io.net.in.ready := false.B

  when (state === s_idle && io.start) {
    state := s_send
  }

  when (sendDone) { state := s_done }
}

class BroadcastTestReceiver(srcmac: Long, ifWidthBits: Int) extends Module {
  val io = IO(new Bundle {
    val net = new StreamIO(ifWidthBits)
    val finished = Output(Bool())
  })

  val s_recv :: s_done :: Nil = Enum(2)
  val state = RegInit(s_recv)

  io.net.out.valid := false.B
  io.net.out.bits.last := DontCare
  io.net.out.bits.data := DontCare
  io.net.out.bits.keep := DontCare
  io.net.in.ready := state === s_recv
  io.finished := state === s_done

  val expectedHeader = EthernetHeader(ETH_BCAST_MAC, srcmac.U, 0.U)
  val expectedPacket = VecInit(expectedHeader.toWords(ifWidthBits) ++ Seq(1, 2, 3, 4).map(_.U(ifWidthBits.W)))
  val (recvCnt, recvDone) = Counter(
    io.net.in.fire(), expectedPacket.size)

  when (recvDone) { state := s_done }

  assert(!io.net.in.valid || io.net.in.bits.data === expectedPacket(recvCnt),
    "BroadcastTest: incorrect data received")

  assert(!recvDone || io.net.in.bits.last,
    "BroadcastTest: Last not asserted for final word")
}

class BroadcastTest(testWidth: Int = 64)(implicit p: Parameters) extends LazyModule {
  val macAddrs = Seq(
    0x554433221100L, 0xBBAA99887766L,
    0x1100FFEEDDCCL, 0x776655443322L)
  val setup = LazyModule(new SwitchTestSetup(macAddrs))
  val netConfig = IceNetConfig(NET_IF_WIDTH_BITS = testWidth)
  val switch = LazyModule(new SimpleSwitch(0, 4, netConfig))
  
  switch.node := setup.node

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    val io = IO(new Bundle with UnitTestIO)
    val sender = Module(new BroadcastTestSender(macAddrs.head, netConfig))
    val receivers = Seq.fill(3) {
      Module(new BroadcastTestReceiver(macAddrs.head, netConfig.NET_IF_WIDTH_BITS))
    }
    val allNets = sender.io.net +: receivers.map(_.io.net)

    switch.module.io.streams.zip(allNets).foreach {
      case (a, b) => a.flipConnect(b)
    }

    sender.io.start := io.start
    io.finished := receivers.map(_.io.finished).reduce(_ && _)
  }
}

class BroadcastTestWrapper(testWidth: Int = 64)(implicit p: Parameters) extends UnitTest {
  val test = Module(LazyModule(new BroadcastTest(testWidth)).module)
  test.io.start := io.start
  io.finished := test.io.finished
}
