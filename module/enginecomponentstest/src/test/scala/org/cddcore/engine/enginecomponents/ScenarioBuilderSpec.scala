package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CddSpec

class ScenarioBuilderSpec extends CddSpec {

  import Scenario._

  "A <situation> produces <result>" should "make a SituationAndResultScenario" in {
    val s = 1 produces "result"
    val expectedDefinedInSourceCodeAt = "(ScenarioBuilderSpec.scala:10)"
    s shouldBe Scenario[Int, String](situation = 1, assertion = EqualsAssertion("result"), reason = SimpleReason("result", expectedDefinedInSourceCodeAt), definedInSourceCodeAt = expectedDefinedInSourceCodeAt)
    s.expectedOption shouldBe Some("result")

  }

  it should "allow the 'by' word to have code generate the result" in {
    val sa = 1 produces "1"
    val sb = sa by (_.toString)
    sa(1) shouldBe ("1")
    sa(2) shouldBe ("1")

    sb(1) shouldBe ("1")
    sb(2) shouldBe ("2")

    sb.copy(reason = sa.reason) shouldBe sa
  }


  it should "have allScenarios just including itself" in {
    val s = (1 produces "result")
    s.allScenarios.toList shouldBe List(s)
  }

  "A <situation> produces <result> because <pf>" should "make a ScenarioWithBecause" in {
    val s: Scenario[Int, String] = 1 produces "result" because { case i if i == 1 => "result" }
    s.situation shouldBe 1
    s.assertion shouldBe EqualsAssertion("result")
    s.isDefinedAt(0) shouldBe false
    s.isDefinedAt(1) shouldBe true
    s(1) shouldBe "result"
    s.expectedOption shouldBe Some("result")
  }

  it should "throw BecauseNotTrueException when the because is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" because { case i if i == 2 => "result" }
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:47) cannot be added because the reason given isn't actually true"
  }

  it should "have allScenarios just including itself" in {
    val s = (1 produces "result") because { case i if i == 1 => "result" }
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw WrongReasonProducesException when the apply method applied to the situation doesn't produce the expected" in {
    intercept[AssertionInvalidException] {
      1 produces "result" because { case i if i == 1 => "wrongResult" }
    }
  }

  "A <situation> produces <result> when < p=> Boolean>" should "make a ScenarioWithWhen" in {
    val s: Scenario[Int, String] = 1 produces "result" when (_ == 1)
    s.situation shouldBe 1
    s.assertion shouldBe EqualsAssertion("result")
    s.isDefinedAt(0) shouldBe false
    s.isDefinedAt(1) shouldBe true
    s(1) shouldBe "result"
    s.expectedOption shouldBe Some("result")
  }


  it should "have allScenarios just including itself" in {
    val s = 1 produces "result" when (_ == 1)
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw BecauseNotTrueException when the when is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" when (_ == 2)
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:80) cannot be added because the reason given isn't actually true"
  }

  it should "allow the 'by' word to have code generate the result" in {
    val sa = 1 produces "1" when (_ == 1)
    val sb = sa by (_.toString)
    sa(1) shouldBe ("1")
    sa(2) shouldBe ("1")

    sb(1) shouldBe ("1")
    sb(2) shouldBe ("2")

    sb.copy(reason = sa.reason) shouldBe sa
  }
}
