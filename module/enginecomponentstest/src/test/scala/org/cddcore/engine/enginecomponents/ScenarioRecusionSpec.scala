package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CddSpec


class ScenarioRecusionSpec extends CddSpec {

  import Scenario._

  "A <situation> produces <result> byRecusion" should "have situation and assertion defined as normal" in {
    val s1 = 3 produces 6 byRecursion { case (engine, i) => i * engine(i - 1) }
    s1.situation shouldBe 3
    s1.assertion shouldBe EqualsAssertion(6)
    val ByRecursionReason(ch, "(ScenarioRecusionSpec.scala:11)") = s1.reason
    ch.fn(_ => 2, 3) shouldBe 6
  }
}