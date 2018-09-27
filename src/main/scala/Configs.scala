package icenet

import chisel3._
import freechips.rocketchip.subsystem.BaseSubsystemConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests

class WithIceNetUnitTestsDefault extends Config((site, here, up) => {
  case NICKey => NICConfig(NET_IF_WIDTH_BITS = 64)
  case UnitTests => (p: Parameters) => {
    Seq(
      Module(new NetworkPacketBufferTest),
      Module(new CreditTrackerTest),
      Module(new BasicSwitchTestWrapper()(p)),
      Module(new BroadcastTestWrapper()(p)),
      Module(new NetworkTapTest),
      Module(new RateLimiterTest),
      Module(new AlignerTest),
      Module(new IceNicSendTestWrapper()(p)),
      Module(new IceNicRecvTestWrapper()(p)),
      Module(new IceNicTestWrapper()(p)))
  }
})

class WithIceNetUnitTestsAll extends Config((site, here, up) => {
  case NICKey => NICConfig(NET_IF_WIDTH_BITS = 64)
  case UnitTests => (p: Parameters) => {
    Seq(
      Module(new NetworkPacketBufferTest(64)),
      Module(new NetworkPacketBufferTest(128)),
      Module(new NetworkPacketBufferTest(256)),
      Module(new NetworkPacketBufferTest(512)),
      Module(new CreditTrackerTest(64)),
      Module(new CreditTrackerTest(128)),
      Module(new CreditTrackerTest(256)),
      Module(new CreditTrackerTest(512)),
      Module(new BasicSwitchTestWrapper(64)(p)),
      Module(new BasicSwitchTestWrapper(128)(p)),
      Module(new BasicSwitchTestWrapper(256)(p)),
      Module(new BasicSwitchTestWrapper(512)(p)),
      Module(new BroadcastTestWrapper(64) (p)),
      Module(new BroadcastTestWrapper(128)(p)),
      Module(new BroadcastTestWrapper(256)(p)),
      Module(new BroadcastTestWrapper(512)(p)),
      Module(new NetworkTapTest(64)),
      Module(new NetworkTapTest(128)),
      Module(new NetworkTapTest(256)),
      Module(new NetworkTapTest(512)),
      Module(new RateLimiterTest),
      Module(new AlignerTest(64)),
      Module(new AlignerTest(128)),
      Module(new AlignerTest(256)),
      Module(new AlignerTest(512)),
      Module(new IceNicSendTestWrapper()(p)),
      Module(new IceNicRecvTestWrapper()(p)),
      Module(new IceNicTestWrapper()(p)))
  }
})

class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTestsAll ++ new BaseSubsystemConfig)
