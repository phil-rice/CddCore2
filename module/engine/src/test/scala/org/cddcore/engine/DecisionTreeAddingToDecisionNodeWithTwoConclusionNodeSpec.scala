package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.NullLifeCycle

class DecisionTreeAddingToDecisionNodeWithTwoConclusionNodeSpec extends CddNonRecursiveSpec[Int, String] {

  import DecisionTreeBuilder._
  import Scenario._

  implicit val lifeCycle = new NullLifeCycle[EngineComponent[Int, String]]

  val mainS = 1 produces "result 1" when (_ == 1)
  val falseS = 2 produces "result 2"

  val dn: DecisionNode[Int, String] = mainS ifFalse falseS

  "adding situations with reasons" should "just modify the conclusion nodes" in {
    val s1 = 1 produces "result 1"
    val s2 = 2 produces "result 2"
    DecisionTree.addOne(mockEngine, dn, s1) shouldBe (mainS ifTrue(mainS, s1) ifFalse falseS)
    DecisionTree.addOne(mockEngine, dn, s2) shouldBe (mainS ifTrue mainS ifFalse(falseS, s2))
  }

}

