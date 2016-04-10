/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.NullLifeCycle


class DecisionTreeLensForSpec extends CddNonRecursiveSpec[Int, String] {

  import DecisionTreeBuilder._
  import Scenario._

  implicit val lifeCycle = new NullLifeCycle[EngineComponent[Int, String]]


  "The decisiontree lensFor method called on a Conclusion Node" should "return a lens pointing to the conclusion node" in {
    val s1 = 1 produces "result"
    val cn1: ConclusionNode[Int, String] = s1
    val lensToDt = cn1.lensFor(mockEngine, s1)
    lensToDt.get(cn1) shouldBe cn1
  }

  "it" should "let the lens returned access and modify the conclusionnode " in {
    val s = 1 produces "result"
    val cn1: ConclusionNode[Int, String] = s
    val cn2: ConclusionNode[Int, String] = 2 produces "result"

    val lensToCn = cn1.lensFor(mockEngine, s)
    lensToCn.get(cn1) shouldBe cn1
    lensToCn.transform(cn1, x => if (x != cn1) throw new RuntimeException else cn2) shouldBe cn2
  }

  "The decisiontree lensFor method called on a Decision Node, a Conclusion node on its truepath and a 'true path' scenario" should "return a lens pointing to the conclusion node on the true path" in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val dt = s1 ifFalse s2

    val lensToDt = dt.lensFor(mockEngine, s3)
    lensToDt.get(dt) shouldBe dt.trueNode
  }

  it should "let the lens returned modify the reached condition once " in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lens = dt.lensFor(mockEngine, s3)
    lens.get(dt) shouldBe dt.trueNode

    lens.transform(dt, { case cn: ConclusionNode[Int, String] => if (cn != dt.trueNode) throw new RuntimeException else cn.withScenario(s3) }) shouldBe (s1 ifTrue(s1, s3) ifFalse s2)
  }

  "The decisiontree lensFor method called on a Decision Node, a Conclusion node on its falsePath and a 'false path' scenario" should "return a lens pointing to the conclusion node on the false path" in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lensToDt = dt.lensFor(mockEngine, s4)
    lensToDt.get(dt) shouldBe dt.falseNode
  }
  it should "let the lens returned modify the reached condition once " in {
    val s1 = 1 produces "odd" when (_ % 2 == 1)
    val s2 = 2 produces "even"
    val s3 = 3 produces "odd"
    val s4 = 4 produces "even"
    val dt = s1 ifFalse s2

    val lens = dt.lensFor(mockEngine, s4)
    lens.get(dt) shouldBe dt.falseNode
    lens.transform(dt, { case cn: ConclusionNode[Int, String] => if (cn != dt.falseNode) throw new RuntimeException else cn.withScenario(s4) }) shouldBe (s1 ifFalse(s2, s4))
  }

}
