package org.cddcore.engine

class ScenarioBuilderSpec extends CddSpec {

  import Scenario._

  "A <situation> produces <result>" should "make a SituationAndResultScenario" in {
    val s = 1 produces "result"
    s shouldBe SituationAndResultScenario(situation = 1, expected = "result", addedException = s.addedException)
    s.definedInSourceCodeAt shouldBe "(ScenarioBuilderSpec.scala:8)"
  }

  "A <situation> produces <result> because <pf>" should "make a ScenarioWithBecause" in {
    val s: ScenarioWithBecause[Int, String] = 1 produces "result" because { case i if i == 1 => "result" }
    s.situation shouldBe 1
    s.expected shouldBe "result"
    s.because.isDefinedAt(0) shouldBe false
    s.because.isDefinedAt(1) shouldBe true
    s.because(1) shouldBe "result"
  }
  "A ScenarioWithBecause" should "throw BecauseNotTrueException when the because is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" because { case i if i == 2 => "result" }
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:23) cannot be added because the reason given isn't actually true"
  }
  "A ScenarioWithBecause" should "throw WrongReasonProducesException when the apply method applied to the situation doesn't produce the expected" in {
    intercept[WrongResultProducedException] {
      1 produces "result" because { case i if i == 1 => "wrongResult" }
    }
  }

  "A <situation> produces <result> because < p=> Boolean>" should "make a ScenarioWithWhen" in {
    val s: ScenarioWithWhen[Int, String] = 1 produces "result" why (_ == 1)
    s.situation shouldBe 1
    s.expected shouldBe "result"
    s.when(0) shouldBe false
    s.when(1) shouldBe true
    s(1) shouldBe "result"
  }

  "A ScenarioWithWhen" should "throw BecauseNotTrueException when the when is not true for the situation" in {
    intercept[ReasonInvalidException] {
      1 produces "result" why (_ == 2)
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:43) cannot be added because the reason given isn't actually true"
  }
}
