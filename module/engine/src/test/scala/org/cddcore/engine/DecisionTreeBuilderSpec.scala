package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.NullLifeCycle


class DecisionTreeBuilderSpec extends CddEngineSpec {

  import DecisionTreeBuilder._
  import Scenario._

  implicit val lifeCycle = new NullLifeCycle[EngineComponent[Int, String]]

  "Scenarios" should " chance to conclusion nodes" in {
    val s1 = 1 produces "result1"
    val cn1: ConclusionNode[Int, String] = s1
    cn1.mainScenario shouldBe s1
    cn1.scenarios shouldBe List()
  }

  "A tuple of scenarios" should "become a conclusion node" in {
    val s1 = 1 produces "result1"
    val s2 = 2 produces "result2" when (_ == 2)
    val s3 = 3 produces "result2" when (_ == 3)
    val cn1: ConclusionNode[Int, String] = (s1, s2)
    cn1.mainScenario shouldBe s1
    cn1.scenarios shouldBe List(s2)

    val cn2: ConclusionNode[Int, String] = (s1, s2, s3)
    cn2.mainScenario shouldBe s1
    cn2.scenarios shouldBe List(s2, s3)
  }

  "a scenario or decision node followed by ifTrue and ifFalse" should "produce a decision tree" in {
    val s1 = 1 produces "result1" when (_ == 1)
    val s2 = 2 produces "result2"

    val dt: DecisionNode[Int, String] = s1 ifTrue s1 ifFalse s2
    dt.mainScenario shouldBe s1
    dt.trueNode shouldBe (s1: ConclusionNode[Int, String])
    dt.falseNode shouldBe (s2: ConclusionNode[Int, String])
  }
  "a scenario or decision node followed by ifFalse" should "produce a decision tree" in {
    val s1 = 1 produces "result1" when (_ == 1)
    val s2 = 2 produces "result2"

    val dt: DecisionNode[Int, String] = s1 ifFalse s2
    dt.mainScenario shouldBe s1
    dt.trueNode shouldBe (s1: ConclusionNode[Int, String])
    dt.falseNode shouldBe (s2: ConclusionNode[Int, String])
  }
}
