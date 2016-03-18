package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{Scenario, EngineComponent}
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

}
