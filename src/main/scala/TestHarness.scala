package icenet

import freechips.rocketchip.config.Parameters
import firrtl.options.{StageMain}
import freechips.rocketchip.system.{RocketChipStage}

class TestHarness(implicit p: Parameters) extends freechips.rocketchip.unittest.TestHarness

object Generator extends StageMain(new RocketChipStage)
