package org.cddcore.core.engine


class ScenarioBuilderSpec extends CddSpec {

  import Scenario._

  "A <situation> produces <result>" should "make a SituationAndResultScenario" in {
    val s = 1 produces "result"
    s.situation shouldBe 1
    s.expected shouldBe "result"
    s.reason shouldBe SimpleReason[Int, String]("result")
    s.definedInSourceCodeAt shouldBe "(ScenarioBuilderSpec.scala:9)"
    s shouldBe Scenario[Int, String](situation = 1, expected = "result", reason = SimpleReason("result"), definedInSourceCodeAt = "(ScenarioBuilderSpec.scala:9)")

  }

  it should "have allScenarios just including itself" in {
    val s = (1 produces "result")
    s.allScenarios.toList shouldBe List(s)
  }

  "A <situation> produces <result> because <pf>" should "make a ScenarioWithBecause" in {
    val s: Scenario[Int, String] = 1 produces "result" because { case i if i == 1 => "result" }
    s.situation shouldBe 1
    s.expected shouldBe "result"
    s.isDefinedAt(0) shouldBe false
    s.isDefinedAt(1) shouldBe true
    s(1) shouldBe "result"
  }

  it should "throw BecauseNotTrueException when the because is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" because { case i if i == 2 => "result" }
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:34) cannot be added because the reason given isn't actually true"
  }
  it should "have allScenarios just including itself" in {
    val s = (1 produces "result") because { case i if i == 1 => "result" }
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw WrongReasonProducesException when the apply method applied to the situation doesn't produce the expected" in {
    intercept[WrongResultProducedException] {
      1 produces "result" because { case i if i == 1 => "wrongResult" }
    }
  }

  "A <situation> produces <result> because < p=> Boolean>" should "make a ScenarioWithWhen" in {
    val s: Scenario[Int, String] = 1 produces "result" when (_ == 1)
    s.situation shouldBe 1
    s.expected shouldBe "result"
    s.isDefinedAt(0) shouldBe false
    s.isDefinedAt(1) shouldBe true
    s(1) shouldBe "result"
  }
  it should "have allScenarios just including itself" in {
    val s = (1 produces "result") when (_ == 1)
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw BecauseNotTrueException when the when is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" when (_ == 2)
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:63) cannot be added because the reason given isn't actually true"
  }
}
