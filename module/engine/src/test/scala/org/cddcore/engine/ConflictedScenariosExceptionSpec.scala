/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
    bigStringsShouldBeEqual(cannotAdd.mainMessage)(
      s"""Scenario defined at $definedAt2 conflicts with $definedAt1
          |Scenario being added is $definedAt2 $summary2
          |Scenario already existing is $definedAt1 $summary1
          |If it was added, would come to result
          |  one""".stripMargin)
    val advice = cannotAdd.advice
    val explaination = cannotAdd.explaination
  }

  "An engine that cannot add a scenario" should "give advice on what to do when the neither have a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one"
      "ac" produces "two"
    })
    import ripped._
    listShouldBeEqualTo(explaination)(
      "The scenario being added comes to the wrong conclusion in the engine being constructed",
      "It is 'conflicting' with an existing scenario.",
      "The 'logic' about that scenario is being applied to the one being added",
      "CDD cannot differentiate between the two because",
      "neither of these scenarios has a reason, and a reason is needed to differentiate them")
    listShouldBeEqualTo(advice)("A reason could be added to either scenario with a 'when' or a 'because'")
  }


  it should "give advice on what to do when the original scenario has a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one" when (_ contains ("a"))
      "ac" produces "two"
    })
    import ripped._
    listShouldBeEqualTo(explaination)(
      "The scenario being added comes to the wrong conclusion in the engine being constructed",
      "It is 'conflicting' with an existing scenario.",
      "The 'logic' about that scenario is being applied to the one being added",
      "CDD cannot differentiate between the two because",
      s"this scenario doesn't have a reason")

    listShouldBeEqualTo(advice)(s"A reason could be added to scenario ${s2.definedInSourceCodeAt} with a 'when' or a 'because'")
  }

  it should "give advice on what to do when the new scenario has a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one"
      "ac" produces "two" when (_ contains ("a"))
    })
    import ripped._
    listShouldBeEqualTo(explaination)(
      "The scenario being added comes to the wrong conclusion in the engine being constructed",
      "It is 'conflicting' with an existing scenario.",
      "The 'logic' about that scenario is being applied to the one being added",
      "CDD cannot differentiate between the two because",
      s"the existing scenario ${s1.definedInSourceCodeAt} doesn't have a reason")

    listShouldBeEqualTo(advice)(s"A reason could be added to scenario ${s1.definedInSourceCodeAt} with a 'when' or a 'because'")
  }

  it should "give advice on what to do when both scenarios have a reason" in {
    val ripped = new Ripped(new Engine[String, String] {
      "ab" produces "one" when (_ contains ("a"))
      "ac" produces "two" when (_ contains ("a"))
    })
    import ripped._
    listShouldBeEqualTo(explaination)(
      "The scenario being added comes to the wrong conclusion in the engine being constructed",
      "It is 'conflicting' with an existing scenario.",
      "The 'logic' about that scenario is being applied to the one being added",
      "CDD cannot differentiate between the two because",
      "both of these scenarios have a reason, but both reasons are true for both scenarios")
    listShouldBeEqualTo(advice)("One or both reasons have to be 'improved', so that they differentiate between the two scenarios")
  }
}
