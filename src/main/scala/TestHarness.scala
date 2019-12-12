package icenet

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.GeneratorApp

class TestHarness(implicit p: Parameters) extends freechips.rocketchip.unittest.TestHarness

object Generator extends GeneratorApp {
  override lazy val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
  generateFirrtl
  generateAnno
}
