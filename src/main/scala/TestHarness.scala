package icenet

import freechips.rocketchip.config.Parameters

class TestHarness(implicit p: Parameters) extends freechips.rocketchip.unittest.TestHarness

object Generator extends testchipip.GeneratorApp {
  generateFirrtl
  generateAnno
}
