/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
