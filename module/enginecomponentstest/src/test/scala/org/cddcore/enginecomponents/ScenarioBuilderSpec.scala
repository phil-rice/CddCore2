/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, NullLifeCycle}

class ScenarioBuilderSpec extends CddSpec {

  import Scenario._

  val mockEngine: Int => String = _ => throw new RuntimeException("Should not call mockEngine")

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  "A <situation> produces <result>" should "make a SituationAndResultScenario" in {
    val s = 1 produces "result"
    val expectedDefinedInSourceCodeAt = "(ScenarioBuilderSpec.scala:15)"
    s.situation shouldBe 1
    s.assertion shouldBe EqualsAssertion("result")
    s.reason shouldBe SimpleReason("result", s.definedInSourceCodeAt)
    s.title shouldBe "1"
    s.comment shouldBe None
    s.definedInSourceCodeAt.toString shouldBe expectedDefinedInSourceCodeAt
    s.expectedOption shouldBe Some("result")

  }
  it should "allow multiple references" in {
    val document1 = Document.internet("someDoc1")
    val document2 = Document.internet("someDoc2")
    val s1 = 1 produces "one" ref document1 ref(document2, "int2")
    s1.references shouldBe List(Reference(document2, Some("int2")), Reference(document1, None))
  }

  it should "allow comments to be added" in {
    (1 produces "result" withComment "someComment").comment shouldBe Some("someComment")
    (1 produces "result" withComment "someComment" because { case 1 => "result" }).comment shouldBe Some("someComment")
    (1 produces "result" because { case 1 => "result" } withComment "someComment").comment shouldBe Some("someComment")
    (1 produces "result" when (_ == 1) withComment "someComment").comment shouldBe Some("someComment")
    (1 produces "result" withComment "someComment" when (_ == 1)).comment shouldBe Some("someComment")

  }
  it should "allow the 'by' word to have code generate the result" in {
    val sa = 1 produces "1"
    val sb = sa by (_.toString)
    sa(mockEngine, 1) shouldBe ("1")
    sa(mockEngine, 2) shouldBe ("1")

    sb(mockEngine, 1) shouldBe ("1")
    sb(mockEngine, 2) shouldBe ("2")

    sb.copy(reason = sa.reason) shouldBe sa
  }


  it should "have allScenarios just including itself" in {
    val s = (1 produces "result")
    s.allScenarios.toList shouldBe List(s)
  }

  "A <situation> produces <result> because <pf>" should "make a ScenarioWithBecause" in {
    val s: Scenario[Int, String] = 1 produces "result" because { case i if i == 1 => "result" }
    s.situation shouldBe 1
    s.assertion shouldBe EqualsAssertion("result")
    s.isDefinedAt(mockEngine, 0) shouldBe false
    s.isDefinedAt(mockEngine, 1) shouldBe true
    s(mockEngine, 1) shouldBe "result"
    s.expectedOption shouldBe Some("result")
  }

  it should "throw BecauseNotTrueException when the because is not true for the situation" in {
    intercept[ReasonInvalidException[_, _]] {
      1 produces "result" because { case i if i == 2 => "result" }
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:71) cannot be added because the reason given isn't valid"
  }

  it should "have allScenarios just including itself" in {
    val s = (1 produces "result") because { case i if i == 1 => "result" }
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw WrongReasonProducesException when the apply method applied to the situation doesn't produce the expected" in {
    intercept[AssertionInvalidException[_, _]] {
      1 produces "result" because { case i if i == 1 => "wrongResult" }
    }
  }

  "A <situation> produces <result> when < p=> Boolean>" should "make a ScenarioWithWhen" in {
    val s: Scenario[Int, String] = 1 produces "result" when (_ == 1)
    s.situation shouldBe 1
    s.assertion shouldBe EqualsAssertion("result")
    s.isDefinedAt(mockEngine, 0) shouldBe false
    s.isDefinedAt(mockEngine, 1) shouldBe true
    s(mockEngine, 1) shouldBe "result"
    s.expectedOption shouldBe Some("result")
  }


  it should "have allScenarios just including itself" in {
    val s = 1 produces "result" when (_ == 1)
    s.allScenarios.toList shouldBe List(s)
  }

  it should "throw BecauseNotTrueException when the when is not true for the situation" in {
    intercept[ReasonInvalidException[_, _]] {
      1 produces "result" when (_ == 2)
    }.getMessage shouldBe "Scenario defined at (ScenarioBuilderSpec.scala:104) cannot be added because the reason given isn't valid"
  }

  it should "allow the 'by' word to have code generate the result" in {
    val sa = 1 produces "1" when (_ == 1)
    val sb = sa by (_.toString)
    sa(mockEngine, 1) shouldBe ("1")
    sa(mockEngine, 2) shouldBe ("1")

    sb(mockEngine, 1) shouldBe ("1")
    sb(mockEngine, 2) shouldBe ("2")

    sb.copy(reason = sa.reason) shouldBe sa
  }
}
