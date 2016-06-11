/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.{NullLifeCycle, Strings}

class DecisionTreeValidationSpec extends CddNonRecursiveSpec[Int, String] with DecisionTreeValidator {

  import DecisionTree.ValidationIssues._
  import Scenario._

  implicit val lifeCycle = new NullLifeCycle[EngineComponent[Int, String]]

  implicit def toSeq[X](x: X) = Seq(x)

  def toSimple(l: TraversableOnce[ValidationReport[Int, String]]) = l.toList.map(vr => Strings.oneLine(vr.scenario.situation + " " + vr.message).trim)

  val s1 = 1 produces "result"
  val s2 = 2 produces "result" when (_ == 2)
  val s3 = 3 produces "result" because { case x => "result" }
  val s4 = 4 produces "result"
  val sProblem = 1 produces "wrong"
  "A decision tree lensValidationChecker protected method" should "do nothing if all scenarios are 'OK'" in {
    toSimple(validateScenarios[Int, String](mockEngine, ConclusionNode(s1, List(s2, s3)), ScenarioIsInCorrectConclusionNodeChecker)) shouldBe List()
  }

  it should "report problems if a scenario is not in the node it would be from evaluating the situation" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)), //note s2 is when (_ ==2) and thus s1 and s3 should be in the false node
      falseNode = ConclusionNode(s4))
    toSimple(validateScenarios[Int, String](mockEngine, tree, ScenarioIsInCorrectConclusionNodeChecker)) shouldBe List(s"1 $scenarioInWrongConclusionNode", s"3 $scenarioInWrongConclusionNode")
  }

  "A decision tree " should "report any situations that don't come to the correct results based on conclusion node" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s1, List(sProblem, s3)),
      falseNode = ConclusionNode(s4))
    toSimple(validateConclusionNodes(mockEngine, tree, scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker)) shouldBe List(s"1 $scenarioComesToWrongConclusionInNode")
  }


  "A decision tree validate method" should "report all issues" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4, List(sProblem)))
    toSimple(validate(mockEngine, tree)) shouldBe List(
      s"1 $scenarioInWrongConclusionNode",
      s"3 $scenarioInWrongConclusionNode",
      s"1 Scenario comes to wrong conclusion in this node")
  }
}
