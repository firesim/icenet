package icenet

import chisel3._
import chisel3.util._
import freechips.rocketchip.unittest.UnitTest
import freechips.rocketchip.util.{DecoupledHelper, UIntIsOneOf}
import testchipip.{StreamIO, StreamChannel}
import IceNetConsts._
import NetworkHelpers._

class ChecksumCalcRequest extends Bundle {
  val check = Bool()
  val start = UInt(16.W)
  val init  = UInt(16.W)
}

class ChecksumRewriteRequest extends Bundle {
  val check  = Bool()
  val offset = UInt(16.W)
  val start  = UInt(16.W)
  val init   = UInt(16.W)
}

class ChecksumCalc(dataBits: Int) extends Module {
  val dataBytes = dataBits / 8

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ChecksumCalcRequest))
    val stream = new StreamIO(dataBits)
    val result = Decoupled(UInt(16.W))
  })

  val csum = Reg(UInt((dataBits + 16).W))
  val check = Reg(Bool())
  val start  = Reg(UInt(16.W))
  val startPos = Reg(UInt(16.W))
  val nextStartPos = startPos + dataBytes.U
  val sumMask = (0 until dataBytes).map { i =>
    io.stream.in.bits.keep(i) && (startPos + i.U) >= start
  }
  val sumData = io.stream.in.bits.data & FillInterleaved(8, sumMask)

  val s_req :: s_stream :: s_fold :: s_result :: Nil = Enum(4)
  val state = RegInit(s_req)

  io.req.ready := state === s_req
  io.stream.out.valid := state === s_stream && io.stream.in.valid
  io.stream.in.ready := state === s_stream && io.stream.out.ready
  io.stream.out.bits := io.stream.in.bits
  io.result.valid := state === s_result
  io.result.bits := csum(15, 0)

  when (io.req.fire) {
    check := io.req.bits.check
    start := io.req.bits.start
    csum := io.req.bits.init
    startPos := 0.U
    state := s_stream
  }

  when (io.stream.in.fire) {
    when (check) {
      csum := csum + sumData
      startPos := nextStartPos
    }

    when (io.stream.in.bits.last) {
      state := Mux(check, s_fold, s_req)
    }
  }

  when (state === s_fold) {
    val upper = csum(15 + dataBits, 16)
    val lower = csum(15, 0)

    when (upper === 0.U) {
      csum := ~lower
      state := s_result
    } .otherwise {
      csum := upper + lower
    }
  }

  when (io.result.fire) { state := s_req }
}

class ChecksumRewrite(dataBits: Int, nBufFlits: Int) extends Module {
  val dataBytes = dataBits / 8

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ChecksumRewriteRequest))
    val stream = new StreamIO(dataBits)
  })

  val reqq = Module(new Queue(new ChecksumRewriteRequest, 2))
  val calc = Module(new ChecksumCalc(dataBits))
  val buffer = Module(new Queue(new StreamChannel(dataBits), nBufFlits))
  val offset = Reg(UInt(16.W))
  val check  = Reg(Bool())
  val csum = Reg(UInt(16.W))

  val reqHelper = DecoupledHelper(
    io.req.valid,
    calc.io.req.ready,
    reqq.io.enq.ready)

  io.req.ready := reqHelper.fire(io.req.valid)
  calc.io.req.valid := reqHelper.fire(calc.io.req.ready)
  calc.io.req.bits.check := io.req.bits.check
  calc.io.req.bits.start := io.req.bits.start
  calc.io.req.bits.init := io.req.bits.init
  reqq.io.enq.valid := reqHelper.fire(reqq.io.enq.ready)
  reqq.io.enq.bits := io.req.bits
  calc.io.stream.in <> io.stream.in
  buffer.io.enq <> calc.io.stream.out

  val byteOffBits = log2Ceil(dataBytes)
  val startPos = Reg(UInt(16.W))
  val nextStartPos = startPos + dataBytes.U
  val baseData = buffer.io.deq.bits.data
  val shiftAmt = Cat((offset - startPos)(byteOffBits-1, 0), 0.U(3.W))
  val dataMask = ~(~0.U(16.W) << shiftAmt)
  val csumShifted = csum << shiftAmt
  val replace = check && (offset >= startPos) && (offset < nextStartPos)
  val outData = Mux(replace, (baseData & dataMask) | csumShifted, baseData)

  val s_req :: s_wait :: s_flush :: Nil = Enum(3)
  val state = RegInit(s_req)

  when (reqq.io.deq.fire) {
    check := reqq.io.deq.bits.check
    offset := reqq.io.deq.bits.offset
    startPos := 0.U
    state := Mux(reqq.io.deq.bits.check, s_wait, s_flush)
  }

  when (calc.io.result.fire) {
    csum := calc.io.result.bits
    state := s_flush
  }

  when (io.stream.out.fire) {
    startPos := nextStartPos
    when (io.stream.out.bits.last) { state := s_req }
  }

  val deqOK = (state === s_flush || nextStartPos <= offset)

  reqq.io.deq.ready := state === s_req
  calc.io.result.ready := state === s_wait

  io.stream.out.valid := buffer.io.deq.valid && deqOK
  buffer.io.deq.ready := io.stream.out.ready && deqOK
  io.stream.out.bits := buffer.io.deq.bits
  io.stream.out.bits.data := outData
}

class ChecksumTest extends UnitTest {
  val offset = 6
  val init = 0x4315
  val start = 2
  val data = Seq(0xdead, 0xbeef, 0x7432, 0x0000, 0xf00d, 0x3163, 0x9821, 0x1543)
  val take = Seq(true,   true,   true,   true,   true,   true,   true,   false)
  val keep = take.map(if (_) 0x3 else 0x0)

  var csum = init + data.zip(take).drop(start/2)
                        .filter(_._2).map(_._1).reduce(_ + _)
  while (csum > 0xffff) {
    csum = (csum >> 16) + (csum & 0xffff)
  }
  csum = ~csum & 0xffff

  val expected = data.take(offset/2) ++
    Seq(csum) ++ data.drop(offset/2+1)

  def seqToVec(seq: Seq[Int], step: Int, nbits: Int) = {
    VecInit((0 until seq.length by step).map { i =>
      Cat((i until (i + step)).map(seq(_).U(nbits.W)).reverse)
    })
  }

  val dataBits = 32
  val dataBytes = dataBits / 8
  val shortsPerFlit = dataBits / 16
  val dataVec = seqToVec(data, shortsPerFlit, 16)
  val expectedVec = seqToVec(expected, shortsPerFlit, 16)
  val keepVec = seqToVec(keep, shortsPerFlit, 2)

  val s_start :: s_req :: s_input :: s_output :: s_done :: Nil = Enum(5)
  val state = RegInit(s_start)

  val rewriter = Module(new ChecksumRewrite(
    dataBits, data.length/shortsPerFlit))

  val (inIdx, inDone) = Counter(rewriter.io.stream.in.fire, dataVec.length)
  val (outIdx, outDone) = Counter(rewriter.io.stream.out.fire, expectedVec.length)

  rewriter.io.req.valid := state === s_req
  rewriter.io.req.bits.check  := true.B
  rewriter.io.req.bits.start  := start.U
  rewriter.io.req.bits.init   := init.U
  rewriter.io.req.bits.offset := offset.U
  rewriter.io.stream.in.valid := state === s_input
  rewriter.io.stream.in.bits.data := dataVec(inIdx)
  rewriter.io.stream.in.bits.keep := keepVec(inIdx)
  rewriter.io.stream.in.bits.last := inIdx === (dataVec.length-1).U
  rewriter.io.stream.out.ready := state === s_output
  io.finished := state === s_done

  when (state === s_start && io.start) { state := s_req }
  when (rewriter.io.req.fire) { state := s_input }
  when (inDone) { state := s_output }
  when (outDone) { state := s_done }

  assert(!rewriter.io.stream.out.valid ||
    rewriter.io.stream.out.bits.data === expectedVec(outIdx),
    "ChecksumTest: got wrong data")

  assert(!rewriter.io.stream.out.valid ||
    rewriter.io.stream.out.bits.keep === keepVec(outIdx),
    "ChecksumTest: got wrong keep")
}

class TCPChecksumOffloadResult extends Bundle {
  val correct = Bool()
  val checked = Bool()
}

class TCPChecksumOffload(dataBits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new StreamChannel(dataBits)))
    val result = Decoupled(new TCPChecksumOffloadResult)
  })

  class FullHeader extends Bundle {
    val tcp = new TCPHeader
    val ipv4 = new IPv4Header
    val eth = new EthernetHeader
  }
  class PseudoHeader extends Bundle {
    val tcp = new TCPHeader
    val length = UInt(16.W)
    val protocol = UInt(8.W)
    val zeros = UInt(8.W)
    val dest_ip = UInt(32.W)
    val source_ip = UInt(32.W)
  }

  val dataBytes = dataBits/8

  val headerBytes = ETH_HEAD_BYTES + IPV4_HEAD_BYTES + TCP_HEAD_BYTES
  val headerWords = headerBytes / dataBytes
  val headerVec = Reg(Vec(headerWords, UInt(dataBits.W)))
  val header = headerVec.asTypeOf(new FullHeader)
  val headerIdx = RegInit(0.U(log2Ceil(headerWords).W))

  val pseudoHeaderBytes = 12 + TCP_HEAD_BYTES
  val pseudoHeaderWords = pseudoHeaderBytes / dataBytes
  val pseudoHeader = Wire(new PseudoHeader)
  val pseudoHeaderVec = pseudoHeader.asTypeOf(
    Vec(pseudoHeaderWords, UInt(dataBits.W)))

  pseudoHeader.tcp := header.tcp
  pseudoHeader.length := htons(ntohs(header.ipv4.total_length) - IPV4_HEAD_BYTES.U)
  pseudoHeader.protocol := header.ipv4.protocol
  pseudoHeader.zeros := 0.U
  pseudoHeader.dest_ip := header.ipv4.dest_ip
  pseudoHeader.source_ip := header.ipv4.source_ip

  require(dataBits >= 16)
  require(headerBytes % dataBytes == 0)
  require(pseudoHeaderBytes % dataBytes == 0)

  val (s_header_in :: s_csum_req ::
       s_header_csum :: s_body_csum ::
       s_passthru :: s_result :: Nil) = Enum(6)
  val state = RegInit(s_header_in)

  val headerOK =
    header.eth.ethType === IPV4_ETHTYPE.U &&
    header.ipv4.protocol === TCP_PROTOCOL.U &&
    header.ipv4.ihl === 5.U
  val headerChannel = Wire(new StreamChannel(dataBits))
  headerChannel.data := pseudoHeaderVec(headerIdx)
  headerChannel.keep := ~0.U(dataBits.W)
  headerChannel.last := false.B

  val resultExpected = RegInit(false.B)

  val csum = Module(new ChecksumCalc(dataBits))

  csum.io.req.valid := state === s_csum_req
  csum.io.req.bits.check := true.B
  csum.io.req.bits.start := 0.U
  csum.io.req.bits.init  := 0.U

  csum.io.stream.in.valid :=
    (state === s_header_csum) || (state === s_body_csum && io.in.valid)
  csum.io.stream.in.bits := Mux(
    state === s_header_csum, headerChannel, io.in.bits)
  csum.io.stream.out.ready := true.B // just ignore output

  io.in.ready := state.isOneOf(s_header_in, s_passthru) ||
                (state === s_body_csum && csum.io.stream.in.ready)

  io.result.valid := state === s_result && (!resultExpected || csum.io.result.valid)
  io.result.bits.correct := csum.io.result.bits === 0.U
  io.result.bits.checked := resultExpected
  csum.io.result.ready := state === s_result && resultExpected && io.result.ready

  when (io.in.fire) {
    when (io.in.bits.last) {
      state := s_result
    } .elsewhen (state === s_header_in) {
      headerVec(headerIdx) := io.in.bits.data
      headerIdx := headerIdx + 1.U
      when (headerIdx === (headerWords-1).U) {
        resultExpected := headerOK
        state := Mux(headerOK, s_csum_req, s_passthru)
      }
    }
  }

  when (csum.io.req.fire) {
    headerIdx := 0.U
    state := s_header_csum
  }

  when (state === s_header_csum && csum.io.stream.in.ready) {
    headerIdx := headerIdx + 1.U
    when (headerIdx === (pseudoHeaderWords-1).U) {
      state := s_body_csum
    }
  }

  when (io.result.fire) {
    headerIdx := 0.U
    resultExpected := false.B
    state := s_header_in
  }
}

class ChecksumTCPVerify extends UnitTest {
  val packets = Seq("""00 00 00 12 6d 00 00 03 00 12 6d 00 00 02 08 00
                      |45 00 00 3c 45 9b 40 00 40 06 9c fb ac 10 00 02
                      |ac 10 00 03 c2 a8 14 51 22 ad 9e d6 00 00 00 00
                      |a0 02 72 10 bf 07 00 00 02 04 05 b4 04 02 08 0a
                      |43 ec e2 58 00 00 00 00 01 03 03 07 00 00 00 00""",
                    """00 00 00 12 6d 00 00 02 00 12 6d 00 00 03 08 06
                      |00 01 08 00 06 04 00 02 00 12 6d 00 00 03 ac 10
                      |00 03 00 12 6d 00 00 02 ac 10 00 02 00 02 00 00""")
  val dataBytes = packets.map(packetStr =>
      packetStr.stripMargin.replaceAll("\n", " ")
               .split(" ").map(BigInt(_, 16)))
  val dataWords = VecInit(dataBytes.flatMap(
    bytes => (0 until bytes.length by 8).map(
      i => Cat(bytes.slice(i, i + 8).map(_.U(8.W)).reverse))))
  val dataLast = VecInit(dataBytes.map(_.length / 8).flatMap(
    nWords => Seq.fill(nWords-1)(false.B) :+ true.B))

  val expectedResults = VecInit(3.U, 2.U)

  val started = RegInit(false.B)
  val inputValid = RegInit(false.B)
  val outputReady = RegInit(false.B)
  val resultReady = RegInit(false.B)

  val offload = Module(new TCPChecksumOffload(NET_IF_WIDTH))

  val (inIdx, inDone) = Counter(offload.io.in.fire, dataWords.length)
  val (resultIdx, resultDone) = Counter(offload.io.result.fire, expectedResults.length)

  offload.io.in.valid := inputValid
  offload.io.in.bits.data := dataWords(inIdx)
  offload.io.in.bits.keep := NET_FULL_KEEP
  offload.io.in.bits.last := dataLast(inIdx)
  offload.io.result.ready := resultReady
  io.finished := started && !inputValid && !resultReady

  when (!started && io.start) {
    started := true.B
    inputValid := true.B
    resultReady := true.B
  }
  when (inDone) { inputValid := false.B }
  when (resultDone) { resultReady := false.B }

  assert(!offload.io.result.valid ||
    offload.io.result.bits.asUInt === expectedResults(resultIdx),
    "ChecksumTCPVerify: checksum was not correct")
}
