package icenet

import chisel3._
import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.unittest.UnitTests

class IceNetUnitTestConfig extends Config((site, here, up) => {
  case UnitTests => (p: Parameters) => {
    Seq(Module(new NetworkPacketBufferTest))
  }
})
