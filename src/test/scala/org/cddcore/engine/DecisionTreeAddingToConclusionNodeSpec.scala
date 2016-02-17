package org.cddcore.engine

import scala.language.implicitConversions

class DecisionTreeAddingToConclusionNodeSpec extends CddSpec {

  import Scenario._

  implicit def toSeq[X](x: X) = Seq(x)


  "A decision tree with one scenario" should "be a single conclusion" in {
    val scenario = "situation" produces "result"
    DecisionTree(scenario) shouldBe ConclusionNode(scenario)
  }

  "A decision tree with two non confliciting scenarios that have no reason" should "be a single conclusion" in {
    val scenario1 = "situation1" produces "result"
    val scenario2 = "situation2" produces "result"
    DecisionTree(Seq(scenario1, scenario2)) shouldBe ConclusionNode(scenario1, List(scenario2))
  }

  "A decision tree with two non confliciting  conclusions, one with a reason" should "be a single conclusion, with the scenario with the reason as the primary" in {
    val scenario1 = "situation1" produces "result"
    val scenario2a = "situation2" produces "result" when (_.contains("situation"))
    val scenario2b = "situation2" produces "result" because { case s if s.contains("situation") => "result" }
    DecisionTree(Seq(scenario1, scenario2a)) shouldBe ConclusionNode(scenario2a, List(scenario1))
    DecisionTree(Seq(scenario1, scenario2b)) shouldBe ConclusionNode(scenario2b, List(scenario1))
    DecisionTree(Seq(scenario2a, scenario1)) shouldBe ConclusionNode(scenario2a, List(scenario1))
    DecisionTree(Seq(scenario2b, scenario1)) shouldBe ConclusionNode(scenario2b, List(scenario1))
  }

  "A decision tree with different conclusions, second with a reason" should "form a decision tree" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2a = "situation2" produces "result2" when (_.contains("2"))
    val scenario2b = "situation2" produces "result2" because { case s if s.contains("2") => "result2" }

    DecisionTree(Seq(scenario1, scenario2a)) shouldBe DecisionNode(scenario2a, falseNode = ConclusionNode(scenario1), trueNode = ConclusionNode(scenario2a))
    DecisionTree(Seq(scenario2a, scenario1)) shouldBe DecisionNode(scenario2a, falseNode = ConclusionNode(scenario1), trueNode = ConclusionNode(scenario2a))
    DecisionTree(Seq(scenario1, scenario2b)) shouldBe DecisionNode(scenario2b, falseNode = ConclusionNode(scenario1), trueNode = ConclusionNode(scenario2b))
    DecisionTree(Seq(scenario2b, scenario1)) shouldBe DecisionNode(scenario2b, falseNode = ConclusionNode(scenario1), trueNode = ConclusionNode(scenario2b))
  }

  "A decision tree with three non confliciting scenarios that have no reason" should "be a single conclusion" in {
    val scenario1 = "situation1" produces "result"
    val scenario2 = "situation2" produces "result"
    val scenario3 = "situation3" produces "result"
    DecisionTree(Seq(scenario1, scenario2, scenario3)) shouldBe ConclusionNode(scenario1, List(scenario2, scenario3))
  }

  "A decision tree with three non confliciting scenarios and only one reason between them" should "be a single conclusion" in {
    val scenario1 = "situation1" produces "result" when (_.contains("situation"))
    val scenario2 = "situation2" produces "result"
    val scenario3 = "situation3" produces "result"
    DecisionTree(Seq(scenario1, scenario2, scenario3)) shouldBe ConclusionNode(scenario1, List(scenario2, scenario3))
    DecisionTree(Seq(scenario2, scenario1, scenario3)) shouldBe ConclusionNode(scenario1, List(scenario2, scenario3))
    DecisionTree(Seq(scenario2, scenario3, scenario1)) shouldBe ConclusionNode(scenario1, List(scenario2, scenario3))
  }


  "A decision tree with different conclusions, one with a reason" should "form a decision tree" in {
    val scenario1a = "situation1a" produces "result1"
    val scenario1b = "situation1b" produces "result1"
    val scenario2a = "situation2a" produces "result2" when (_.contains("2"))
    val scenario2b = "situation2b" produces "result2" because { case s if s.contains("2") => "result2" }
    val scenario2c = "situation2c" produces "result2"

    DecisionTree(Seq(scenario1a, scenario1b, scenario2a, scenario2c)) shouldBe
      DecisionNode(scenario2a, falseNode = ConclusionNode(scenario1a, List(scenario1b)), trueNode = ConclusionNode(scenario2a, List(scenario2c)))

    DecisionTree(Seq(scenario1a, scenario1b, scenario2b, scenario2c)) shouldBe
      DecisionNode(scenario2b, falseNode = ConclusionNode(scenario1a, List(scenario1b)), trueNode = ConclusionNode(scenario2b, List(scenario2c)))
  }

}
