package org.cddcore.engine

class DecisionTreeSpec extends CddSpec {

  import Scenario._

  implicit def toSeq[X](x: X) = Seq(x)


  "A decision tree with one scenario" should "be a single conclusion" in {
    val scenario = "situation" produces "result"
    DecisionTree(scenario) shouldBe ConclusionNode(scenario)
  }

  "A decision tree with identical conclusions" should "be a single conclusion" in {
    val scenario1 = "situation1" produces "result"
    val scenario2 = "situation2" produces "result"
    DecisionTree(Seq(scenario1, scenario2)) shouldBe ConclusionNode(scenario1, List(scenario2))
  }

  "A decision tree with different conclusions, second with a because" should "form a decision tree" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2" when (_.contains("2"))

    DecisionTree(Seq(scenario1, scenario2)) shouldBe
      DecisionNode(scenario2, falseNode = ConclusionNode(scenario1), trueNode = ConclusionNode(scenario2))

  }
  "A decision tree with different conclusions, first with a because" should "form a decision tree" in {
    val scenario1 = "situation1" produces "result1" when (_.contains("1"))
    val scenario2 = "situation2" produces "result2"

    DecisionTree(Seq(scenario1, scenario2)) shouldBe
      DecisionNode(scenario1,  trueNode = ConclusionNode(scenario1), falseNode = ConclusionNode(scenario2))
  }

  

}
