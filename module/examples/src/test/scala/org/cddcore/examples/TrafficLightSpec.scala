package org.cddcore.examples

import org.cddcore.core.engine.CddSpec

class TrafficLightSpec extends CddSpec {

  val decide = new TrafficLightEngine().decide
  "Traffic lights" should "behave" in {
    decide(TrafficLight(red = true)) shouldBe "Stop"
    decide(TrafficLight(green = true)) shouldBe "Go"
  }
  it should "have 4 scenarios" in {
    decide.allScenarios.size shouldBe 4
    decide.validate
  }
}
