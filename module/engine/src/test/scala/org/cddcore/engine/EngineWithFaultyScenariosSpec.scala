package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{CannotAddScenarioException, Scenario, EngineComponent}
import org.cddcore.utilities.ChildLifeCycle

class EngineWithFaultyScenariosSpec extends CddEngineSpec {

  import Scenario._


  "An Engine" should "Remember the fault scenarios - bad when" in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" when (_ == 2))
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:12)"
    errors shouldBe Map(s -> "ReasonInvalidException")
  }

  it should "Remember the fault scenarios - bad because" in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" because { case _ => "duff" })
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:18)"
    errors shouldBe Map(s -> "AssertionInvalidException")
  }

  it should "Handle multiple errors by only remembering the first " in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" when (_ == 2) because { case _ => "duff" })
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:24)"
    errors shouldBe Map(s -> "ReasonInvalidException")
  }

  it should "throw an exception" in {
    val e = new Engine[Int, String] {
      1 produces "one"
      2 produces "two"
      3 produces "three"
      4 produces "four"
      5 produces "five"
      6 produces "six"
    }
    e.hierarchyBuilder.holder.errors.size shouldBe 0
    e.decisionTree
    e.hierarchyBuilder.holder.errors.size shouldBe 5
    val ex = intercept[CannotAddScenarioException[Int,String]](e(1))
    withClue(ex.getMessage)(ex  .getMessage.contains("2 produces two") shouldBe true)
  }
  it should "throw an exception with the first broken scenario when the engine is used" in {
    fail
  }
}
