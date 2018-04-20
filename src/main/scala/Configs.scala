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
      //Module(new CreditTrackerTest),
      //Module(new BasicSwitchTestWrapper()(p)),
      //Module(new BroadcastTestWrapper()(p)),
      Module(new NetworkTapTest),
      Module(new RateLimiterTest),
      Module(new AlignerTest),
      Module(new IceNicSendTestWrapper()(p)),
      Module(new IceNicRecvTestWrapper()(p)),
      Module(new IceNicTestWrapper()(p)))
  }
})

class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTests ++ new BaseSubsystemConfig)
