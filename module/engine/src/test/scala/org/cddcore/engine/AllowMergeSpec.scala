/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine


class AllowMergeSpec extends CddEngineSpec {

  def checkExplaination[P, R](e: AddingWithRedundantReason[P, R]) = {
    listShouldBeEqualTo(e.explaination)(s"The creator of the engine has given a reason, but CDD doesn't need it",
      "CDD could add the new scenario to the same place as the original one, but then",
      "the reason given would be lost, and that could create errors later",
      "You need to tell CDD what to do.",
      "Using 'allows merge' means 'use the logical OR of both given reasons'")
  }

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

    checkExplaination(e)

    listShouldBeEqualTo(e.advice)(
      "Both scenarios would need 'allows merge' adding to them",
      s"This scenario ${s2.definedInSourceCodeAt} could have its reason removed")
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

    checkExplaination(e)

    listShouldBeEqualTo(e.advice)(
      s"This scenario $definedAt2 could have 'allows merge' added to it",
      s"This scenario ${s2.definedInSourceCodeAt} could have its reason removed")
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
    checkExplaination(e)
    listShouldBeEqualTo(e.advice)(
      s"The original scenario $definedAt1 could have 'allows merge' added to it",
      s"This scenario ${s2.definedInSourceCodeAt} could have its reason removed")
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
