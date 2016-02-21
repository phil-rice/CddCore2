package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CddSpec


class ScenarioRecusionSpec extends CddSpec {

  import Scenario._

  "A <situation> produces <result> byRecusion" should "have situation and assertion defined as normal" in {
    val s1 = 3 produces 6 byRecursion { (engine, i) => i * engine(i - 1) }
    s1.situation shouldBe 3
    s1.assertion shouldBe EqualsAssertion(6)
  }
}
