package org.cddcore.engine

import org.cddcore.utilities.Strings


class ConflictedScenariosExceptionSpec extends CddEngineSpec {

  class Ripped[P, R](e: Engine[P, R]) {
    val scenarios = e.allScenarios.toSeq
    val Seq(s1, s2) = scenarios
    val Seq(definedAt1, definedAt2) = scenarios.map(_.definedInSourceCodeAt.toString)
    val Seq(summary1, summary2) = scenarios.map(_.summary)
    e.buildDecisionTree
    //    e.apply(1) shouldBe "one"
    e.errors.size shouldBe 1
    val cannotAdd = e.errors(s2).asInstanceOf[ConflictingScenariosException[P, R]]
    cannotAdd.existing shouldBe s1
    cannotAdd.scenario shouldBe s2
    cannotAdd.actual shouldBe "one"
    withClue("MainMessage is" + cannotAdd.mainMessage)(cannotAdd.mainMessage shouldBe
      s"""Scenario defined at $definedAt2 conflicts with $definedAt1
          |Scenario being added is $definedAt2 $summary2
          |Scenario already existing is $definedAt1 $summary1
          |If it was added, would come to result
          |  one""".stripMargin)
    cannotAdd.getMessage should startWith(cannotAdd.mainMessage)
    val advice = cannotAdd.advice
  }

  "An engine that cannot add a scenario" should "give advice on what to do when the neither have a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one"
      "ac" produces "two"
    })
    ripped.advice should contain theSameElementsAs List("Neither of these scenarios has a reason. A reason could be added to either scenario with a 'when' or a 'because'")
  }

  it should "give advice on what to do when the original scenario has a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one" when (_ contains ("a"))
      "ac" produces "two"
    })
    import ripped._
    advice should contain theSameElementsAs List(
      s"The reason it currently comes to that conclusion is",
      s1.reason.prettyDescription,
      s"A reason could be added to scenario $definedAt2 with a 'when' or a 'because'")
  }

  it should "give advice on what to do when the new scenario has a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one"
      "ac" produces "two" when (_ contains ("a"))
    })
    import ripped._
    advice should contain theSameElementsAs List(
      s"The reason it currently comes to that conclusion is",
      s2.reason.prettyDescription,
      s"A reason could be added to scenario $definedAt1 with a 'when' or a 'because'")
  }

  it should "give advice on what to do when both scenarios have a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one" when (_ contains ("a"))
      "ac" produces "two" when (_ contains ("a"))
    })
    import ripped._
    advice should contain theSameElementsAs List(
      "The two scenarios come to different conclusions. Both have a reason, but both reasons are true for both scenario",
      "One or both reasons have to be 'improved', so that they differentiate between the two scenarios")
  }
}
