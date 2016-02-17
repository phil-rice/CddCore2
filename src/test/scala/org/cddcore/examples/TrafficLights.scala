package org.cddcore.examples

import org.cddcore.engine.{Engine2, Engine, CddSpec}

case class TrafficLight(red: Boolean = false, orange: Boolean = false, green: Boolean = false)

class TrafficLightSpec extends CddSpec {
  //This isn't complex enough to really need use cases
  val decide = new Engine2[TrafficLight, String]() {
    useCase("Cars need to obey traffic signals") {
      TrafficLight(red = true) produces "Stop"
      TrafficLight(red = true, orange = true) produces "Stop"
      TrafficLight(green = true) produces "Go" when (_.green)
      TrafficLight(orange = true) produces "Stop"
    }
  }
  "Traffic lights" should "behave" in {
    decide(TrafficLight(red=true)) shouldBe "Stop"
    decide(TrafficLight(green=true)) shouldBe "Go"
  }
  it should "have 4 scenarios" in {
    decide.allScenarios.size shouldBe 4
  }
}
