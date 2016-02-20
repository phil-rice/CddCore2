package org.cddcore.engine.enginecomponents

import org.cddcore.engine.enginecomponents.Scenario
import org.cddcore.utilities.{CddSpec, NullLifeCycle}


class ScenarioBuilderWithAssertionsSpec extends CddSpec {

  implicit val pToScenario = Scenario.pToScenarioBuilder[Int, String] _

  implicit val lifeCycle = new NullLifeCycle[Scenario[Int, String]]
  val something = Scenario.something[String]

  "A scenario" should "be able to express a result assertion instead of just equality" in {
    val fn = (s: String) => s.contains("result")
    val s = 1 produces something where fn
    s.assertion shouldBe ResultAssertion(fn)
  }
}
