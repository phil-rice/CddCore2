package org.cddcore.engine


class AllowMergeSpec extends CddEngineSpec {

  "Scenarios which don't allow merging" should "cause an AddingWithRedundantReason exception" in {
    val engine = new Engine[String, Int] {
      "abc" produces 1 when (_ contains "a")
      "ab" produces 1 when (_ contains "b")
    }
    val Seq(s1, s2) = engine.allScenarios

    engine.decisionTree shouldBe ConclusionNode(s1)
    val e = intercept[AddingWithRedundantReason[String, Int]] {
      engine("abc")
    }
    e.scenario shouldBe s2
    e.existing shouldBe s1

    e.advice should contain theSameElementsAs List(
      "Both scenarios could have 'allow merge' added to them",
      "You could remove the reason from one of them")
  }

  they should "still cause AddingWithRedundantReason even if existing one has allowMerge" in {
    val engine = new Engine[String, Int] {
      "abc" produces 1 when (_ contains "a") allows merge
      "ab" produces 1 when (_ contains "b")
    }
    val Seq(s1, s2) = engine.allScenarios
    val Seq(definedAt1, definedAt2) = engine.allScenarios.toSeq.map(_.definedInSourceCodeAt.toString)

    engine.decisionTree shouldBe ConclusionNode(s1)
    val e = intercept[AddingWithRedundantReason[String, Int]] {
      engine("abc")
    }
    e.scenario shouldBe s2
    e.existing shouldBe s1
    e.advice should contain theSameElementsAs List(
      s"The original scenario at $definedAt2 could have 'allow merge added to it",
      "You could remove the reason from one of them")
  }

  they should "still cause AddingWithRedundantReason even if new one has allowMerge" in {
    val engine = new Engine[String, Int] {
      "abc" produces 1 when (_ contains "a")
      "ab" produces 1 when (_ contains "b") allows merge
    }
    val Seq(s1, s2) = engine.allScenarios
    val Seq(definedAt1, definedAt2) = engine.allScenarios.toSeq.map(_.definedInSourceCodeAt.toString)
    s1.canMerge shouldBe false
    s2.canMerge shouldBe true

    engine.decisionTree shouldBe ConclusionNode(s1)
    val e = intercept[AddingWithRedundantReason[String, Int]] {
      engine("abc")
    }
    e.scenario shouldBe s2
    e.existing shouldBe s1
    e.advice should contain theSameElementsAs List(
      s"The new scenario at $definedAt2 could have 'allow merge added to it",
      "You could remove the reason from one of them")
  }

  they should "merge if both scenarios allowMerge" in {
    val engine = new Engine[String, Int] {
      "abc" produces 1 when (_ contains "a") allows merge
      "ab" produces 1 when (_ contains "b") allows merge
    }
    val Seq(s1, s2) = engine.allScenarios
    s1.canMerge shouldBe true
    s2.canMerge shouldBe true

    val cn: ConclusionNode[String, Int] = engine.decisionTree.asInstanceOf[ConclusionNode[String, Int]]
    cn.isDefinedAt(engine, "a") shouldBe true
    cn.isDefinedAt(engine, "b") shouldBe true
  }
}
