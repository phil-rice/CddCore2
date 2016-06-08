package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, NullLifeCycle}


class ScenarioAllowMergeSpec extends CddSpec {

  import Scenario._

  val mockEngine: Int => String = _ => throw new RuntimeException("Should not call mockEngine")

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  "a situation produces result" should "allow an allowMerge" in {
    val s1 = 1 produces "a"
    s1.canMerge shouldBe false
    val s2 = 1 produces "a" allows Merge
    s2.canMerge shouldBe true
  }

}
