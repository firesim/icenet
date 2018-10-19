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
    Seq(64, 128, 256, 512).flatMap( n => Seq( Module(new AlignerTest(n)) ) )
    /*
    Seq(64, 128, 256, 512).flatMap( 
       n => Seq( Module(new NetworkPacketBufferTest(n)),   // Good
                 Module(new CreditTrackerTest(n)),         // Good 
                 Module(new BasicSwitchTestWrapper(n)(p)), // Good 
                 Module(new BroadcastTestWrapper(n)(p)),   // Broken
                 Module(new NetworkTapTest(n)),            //
                 Module(new AlignerTest(n)),               //
                 Module(new StreamShifterTest(n)),         // Good
                 Module(new RateLimiterTest(n)) ) ++       // 
    Seq( Module(new IceNicSendTestWrapper()(p)), //Note that the general NIC tests use the NICConfig params
         Module(new IceNicRecvTestWrapper()(p)),
         Module(new IceNicTestWrapper()(p)) ) )
    */
  }
})

class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTestsAll ++ new BaseSubsystemConfig)
