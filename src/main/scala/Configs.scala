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
  case NICKey => NICConfig(NET_IF_WIDTH_BITS = 128)
  case UnitTests => (p: Parameters) => {
    Seq( //Module(new AlignerTest(64)),
      Module(new StreamShifterTest(64)))
    /*Seq(64, 128, 256, 512).flatMap( 
       n => Seq( Module(new NetworkPacketBufferTest(n)),
                 Module(new CreditTrackerTest(n)),
                 Module(new BasicSwitchTestWrapper(n)(p)),
                 Module(new BroadcastTestWrapper(n)(p)),
                 Module(new NetworkTapTest(n)), 
                 Module(new AlignerTest(n)),
                 Module(new AlignDataToAddrTest(n)) )) ++
    Seq( Module(new RateLimiterTest),
         Module(new IceNicSendTestWrapper()(p)), //Note that the general tests use the NICConfig params
         Module(new IceNicRecvTestWrapper()(p)),
         Module(new IceNicTestWrapper()(p)) )
    */
  }
})

class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTestsAll ++ new BaseSubsystemConfig)
