/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.NullLifeCycle

class DecisionTreeValidationSpec extends CddNonRecursiveSpec[Int, String] with DecisionTreeValidator {

  import DecisionTree.ValidationIssues._
  import Scenario._

  implicit val lifeCycle = new NullLifeCycle[EngineComponent[Int, String]]
  implicit def toSeq[X](x: X) = Seq(x)

  val s1 = 1 produces "result"
  val s2 = 2 produces "result" when (_ == 2)
  val s3 = 3 produces "result" because { case x => "result" }
  val s4 = 4 produces "result"
  val sProblem = 1 produces "wrong"
  "A decision tree lensValidationChecker protected method" should "do nothing if all scenarios are 'OK'" in {
    validateScenarios[Int, String](mockEngine, ConclusionNode(s1, List(s2, s3)), lensValidationChecker).toList shouldBe List()
  }

  it should "report problems if a scenario is not in the node it would be from evaluating the situation" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4))
    validateScenarios[Int, String](mockEngine, tree, lensValidationChecker).toList shouldBe List(
      ValidationReport(lensReportsWrongScenario, s1),
      ValidationReport(lensReportsWrongScenario, s3))
  }

  "A decision tree scenarioInConclusionNodeChecker" should "do nothing is all scenarios in their correct place" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2),
      falseNode = ConclusionNode(s4, List(s1, s3)))
    validateConclusionNodes(mockEngine, tree, scenarioInConclusionNodeChecker).toList shouldBe List()

  }
  it should "report problems if a scenario in a conclusion node shouldn't be there " in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4))
    validateConclusionNodes(mockEngine, tree, scenarioInConclusionNodeChecker).toList shouldBe List(
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s1),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s3))
  }

  "A decision tree " should "report any situations that don't come to the correct results based on conclusion node" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s1, List(sProblem, s3)),
      falseNode = ConclusionNode(s4))
    validateConclusionNodes(mockEngine, tree, scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker).toList shouldBe List(
      ValidationReport(scenarioComesToWrongConclusionInNode, sProblem)
    )
  }

  it should "report any situations that don't come to the expected result " in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s1, List(sProblem, s3)),
      falseNode = ConclusionNode(s4))
    validateScenarios[Int, String](mockEngine, tree, scenarioComesToCorrectAnswer).toList shouldBe List(
      ValidationReport(scenarioComesToWrongConclusion+"\nActual value is result\n", sProblem))
  }

  "A decision tree validate method" should "report all issues" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4, List(sProblem)))
    validate(mockEngine, tree) shouldBe List(
      ValidationReport(lensReportsWrongScenario, s1),
      ValidationReport(lensReportsWrongScenario, s3),
      ValidationReport(scenarioComesToWrongConclusion+"\nActual value is result\n", sProblem),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s1),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s3),
      ValidationReport(scenarioComesToWrongConclusionInNode, sProblem))
  }
}
