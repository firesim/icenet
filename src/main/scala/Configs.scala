package icenet

import chisel3._
import freechips.rocketchip.subsystem.BaseSubsystemConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests

class WithIceNetUnitTests extends Config((site, here, up) => {
  case NICKey => NICConfig()
  case UnitTests => (p: Parameters) => {
    Seq(
      Module(new NetworkPacketBufferTest),
      Module(new PauserTest),
      Module(new NetworkTapTest),
      Module(new RateLimiterTest),
      Module(new AlignerTest),
      Module(new ChecksumTest),
      Module(new ChecksumTCPVerify),
      Module(new IceNicSendTestWrapper()(p)),
      Module(new IceNicRecvTestWrapper()(p)),
      Module(new IceNicTestWrapper()(p)),
      Module(new MisalignedTestWrapper()(p)))
  }
})

class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTests ++ new BaseSubsystemConfig)
