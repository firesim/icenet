package icenet

import chisel3._
import freechips.rocketchip.subsystem.BaseSubsystemConfig
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests

/**
 * Original set of unit tests with their default values
 */
class WithIceNetUnitTestsDefault extends Config((site, here, up) => {
  case NICKey => NICConfig(NET_IF_WIDTH_BITS = 64)
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

/**
 * Complete set of unit tests
 */
class WithIceNetUnitTestsAll extends Config((site, here, up) => {
  case NICKey => NICConfig(NET_IF_WIDTH_BITS = 64)
  case UnitTests => (p: Parameters) => {
    Seq(64, 128, 256, 512).flatMap(
         n => Seq( Module(new NetworkPacketBufferTest(n)),
                   Module(new CreditTrackerTest(n)),
                   Module(new BasicSwitchTestWrapper(n)(p)),
                   Module(new BroadcastTestWrapper(n)(p)),
                   Module(new NetworkTapTest(n)),
                   Module(new RateLimiterTest(n)) ) ++
              Seq.tabulate(n/16)(m => m * 2).flatMap( // randomized testing for the new module that sweeps through all shift/size combos
                   l => Seq( Module(new StreamShifterParameterizedTest(n, l)) ) ) ) ++
    Seq( Module(new AlignerTest),                // note: that these are not parameterized since you have to specify
         Module(new StreamShifterTest),          // exact data for tests
         Module(new StreamShifterZeroShiftTest),
         Module(new IceNicSendTestWrapper()(p)), // note: that the general NIC tests use the NICConfig params thus they
         Module(new IceNicRecvTestWrapper()(p)), //       will break if the netIfWidthBits is greater than 64
         Module(new IceNicTestWrapper()(p)) )
  }
})

/**
 * Create config with complete set of unit tests
 */
class IceNetUnitTestConfig extends Config(
  new WithIceNetUnitTestsAll ++ new BaseSubsystemConfig)
