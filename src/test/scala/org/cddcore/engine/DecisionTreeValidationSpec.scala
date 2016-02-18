package org.cddcore.engine


class DecisionTreeValidationSpec extends CddSpec with DecisionTreeValidator {

  import Scenario._
  import DecisionTree.ValidationIssues._

  implicit def toSeq[X](x: X) = Seq(x)

  val s1 = 1 produces "result"
  val s2 = 2 produces "result" when (_ == 2)
  val s3 = 3 produces "result" because { case x => "result" }
  val s4 = 4 produces "result"
  val sProblem = 1 produces "wrong"
  "A decision tree lensValidationChecker protected method" should "do nothing if all scenarios are 'OK'" in {
    validateScenarios[Int, String](ConclusionNode(s1, List(s2, s3)), lensValidationChecker).toList shouldBe List()
  }

  it should "report problems if a scenario is not in the node it would be from evaluating the situation" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4))
    validateScenarios[Int, String](tree, lensValidationChecker).toList shouldBe List(
      ValidationReport(lensReportsWrongScenario, s1),
      ValidationReport(lensReportsWrongScenario, s3))
  }

  "A decision tree scenarioInConclusionNodeChecker" should "do nothing is all scenarios in their correct place" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2),
      falseNode = ConclusionNode(s4, List(s1, s3)))
    validateConclusionNodes(tree, scenarioInConclusionNodeChecker).toList shouldBe List()

  }
  it should "report problems if a scenario in a conclusion node shouldn't be there " in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4))
    validateConclusionNodes(tree, scenarioInConclusionNodeChecker).toList shouldBe List(
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s1),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s3))
  }

  "A decision tree " should "report any situations that don't come to the correct results based on conclusion node" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s1, List(sProblem, s3)),
      falseNode = ConclusionNode(s4))
    validateConclusionNodes(tree, scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker).toList shouldBe List(
      ValidationReport(scenarioComesToWrongConclusionInNode, sProblem)
    )
  }

  it should "report any situations that don't come to the expected result " in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s1, List(sProblem, s3)),
      falseNode = ConclusionNode(s4))
    validateScenarios[Int, String](tree, scenarioComesToCorrectAnswer).toList shouldBe List(
      ValidationReport(scenarioComesToWrongConclusion, sProblem))
  }

  "A decision tree validate method" should "report all issues" in {
    val tree = DecisionNode(s2,
      trueNode = ConclusionNode(s2, List(s1, s3)),
      falseNode = ConclusionNode(s4, List(sProblem)))
    validate(tree) shouldBe List(
      ValidationReport(lensReportsWrongScenario, s1),
      ValidationReport(lensReportsWrongScenario, s3),
      ValidationReport(scenarioComesToWrongConclusion, sProblem),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s1),
      ValidationReport(scenarioIsNotDefinedAtConclusionNode, s3),
      ValidationReport(scenarioComesToWrongConclusionInNode, sProblem))
  }
}
