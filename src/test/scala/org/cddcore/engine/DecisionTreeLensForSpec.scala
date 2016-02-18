package org.cddcore.engine

class DecisionTreeLensForSpec extends CddSpec {

  import DecisionTreeBuilder._
  import Scenario._

  "The decisiontree lensFor method called on a Conclusion Node" should "return a lens pointing to the conclusion node" in {
    val s1 = 1 produces "result"
    val cn1: ConclusionNode[Int, String] = s1
    val lensToDt = cn1.lensFor(s1)
    lensToDt.get(cn1) shouldBe cn1
  }

  "it" should "let the lens returned access and modify the conclusionnode " in {
    val s = 1 produces "result"
    val cn1: ConclusionNode[Int, String] = s
    val cn2: ConclusionNode[Int, String] = 2 produces "result"

    val lensToCn = cn1.lensFor(s)
    lensToCn.get(cn1) shouldBe cn1
    lensToCn.transform(cn1, x => if (x != cn1) throw new RuntimeException else cn2) shouldBe cn2
  }

  "The decisiontree lensFor method called on a Decision Node, a Conclusion node on its truepath and a 'true path' scenario" should "return a lens pointing to the conclusion node on the true path" in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val dt = s1 ifFalse s2

    val lensToDt = dt.lensFor(s3)
    lensToDt.get(dt) shouldBe dt.trueNode
  }

  it should "let the lens returned modify the reached condition once " in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lens = dt.lensFor(s3)
    lens.get(dt) shouldBe dt.trueNode

    lens.transform(dt, { case cn: ConclusionNode[Int, String] => if (cn != dt.trueNode) throw new RuntimeException else cn.withScenario(s3) }) shouldBe (s1 ifTrue(s1, s3) ifFalse s2)
  }

  "The decisiontree lensFor method called on a Decision Node, a Conclusion node on its falsePath and a 'false path' scenario" should "return a lens pointing to the conclusion node on the false path" in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lensToDt = dt.lensFor(s4)
    lensToDt.get(dt) shouldBe dt.falseNode
  }
  it should "let the lens returned modify the reached condition once " in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lens = dt.lensFor(s4)
    lens.get(dt) shouldBe dt.falseNode
    lens.transform(dt, { case cn: ConclusionNode[Int, String] => if (cn != dt.falseNode) throw new RuntimeException else cn.withScenario(s4) }) shouldBe (s1 ifFalse(s2, s4))
  }

}
