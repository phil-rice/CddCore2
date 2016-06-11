/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.Scenario

class EngineWithFaultyScenariosSpec extends CddEngineSpec {

  import Scenario._

  "An Engine" should "Remember the fault scenarios - bad when" in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" when (_ == 2))
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:11)"
    errors shouldBe Map(s -> "ReasonInvalidException")
  }

  it should "Remember the fault scenarios - bad because" in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" because { case _ => "duff" })
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:17)"
    errors shouldBe Map(s -> "AssertionInvalidException")
  }

  it should "Handle multiple errors by only remembering the first " in {
    val (e, s, errors) = toErrors[Int, String](implicit clc => 1 produces "one" when (_ == 2) because { case _ => "duff" })
    s.toString shouldBe "Scenario(1 produces one JustBecause)/(EngineWithFaultyScenariosSpec.scala:23)"
    errors shouldBe Map(s -> "ReasonInvalidException")
  }

  it should "throw an exception when used" in {
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
    val ex = intercept[ConflictingScenariosException[Int, String]](e(1))
    withClue(ex.getMessage)(ex.getMessage.contains("2 produces two") shouldBe true)
  }

  it should "let the decision tree be used (so that CddUnit can evaluate the scenarios)" in {
    val e = new Engine[Int, String] {
      1 produces "one"
      2 produces "two"
      3 produces "three"
      4 produces "four"
      5 produces "five"
      6 produces "six"
    }
    e.decisionTree(e, 1) shouldBe "one"
    e.decisionTree(e, 2) shouldBe "one"
  }

  it should "add scenarios that cause exceptions in other scenario's conditions clauses to the exceptions lists" in {
    val e = new RuntimeException("someMessage")
    val engine = new Engine[Int, String] {
      1 produces "result" because {
        case s if s == 1 => "result"
        case _ if (throw e) => "result"
      }
      2 produces "result"
    }

    val Seq(s1, s2) = engine.allScenarios
    //    s1.situation shouldBe 1
    //    s2.situation shouldBe 2
    //    s1.reason.isDefinedAt(engine, 2) shouldBe false
    //    s1.isDefinedAt(engine, 2) shouldBe false
    intercept[RuntimeException](s1.isDefinedAt(engine, 2)) shouldBe e
    engine.decisionTree shouldBe ConclusionNode(s1)
    engine.errors.size shouldBe 1
    val exception = engine.errors(s2).asInstanceOf[ScenarioCausesExceptionInOtherScenariosWhenClause[Int, String]]
    withClue(exception) {
      exception.getCause shouldBe e
      exception.scenario shouldBe s2
      exception.originalScenario shouldBe s1
      bigStringsShouldBeEqual(exception.getMessage)(
        """The scenario defined at (EngineWithFaultyScenariosSpec.scala:64) caused an exception when evaluating the condition of the scenario defined at Scenario(1 produces result because {case (s @ _) if s.==(1) => "result"})/(EngineWithFaultyScenariosSpec.scala:60)
           |This scenario is Scenario(2 produces result JustBecause)/(EngineWithFaultyScenariosSpec.scala:64)
           |Original scenario is Scenario(1 produces result because {case (s @ _) if s.==(1) => "result"})/(EngineWithFaultyScenariosSpec.scala:60)""".stripMargin)
    }
  }
}
