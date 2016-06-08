package org.cddcore.engine

import org.cddcore.enginecomponents.Scenario
import org.cddcore.utilities.{CddSpec, NullLifeCycle}

class ConclusionNodeSpec extends CddSpec {

  import Scenario._

  implicit def lifeCycle[C] = new NullLifeCycle[C]

  val mockEngine = (s: String) => 1
  "A conclusion node" should "have is defined at as the aggregate of the scenarios" in {
    val s1 = "abc" produces 1 when (_ contains "a")
    val s2 = "ab" produces 2 when (_ contains "b")

    val cn = ConclusionNode(s1, List(s2))

    cn.isDefinedAt(mockEngine, "a") shouldBe true
    cn.isDefinedAt(mockEngine, "b") shouldBe true
    cn.isDefinedAt(mockEngine, "c") shouldBe false
  }

  it should "apply the suitable scenario" in {
    val s1 = "abc" produces 1 when (_ contains "a")
    val s2 = "ab" produces 2 when (_ contains "b")

    val cn = ConclusionNode(s1, List(s2))

    cn(mockEngine, "a") shouldBe 1
    cn(mockEngine, "b") shouldBe 2
    val e = intercept[EngineIsNotDefined](cn(mockEngine, "c"))
  }
}
