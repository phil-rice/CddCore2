/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

class EngineTraceSpec extends CddEngineSpec {

  val e = new Engine[Int, String]("e") {
    1 produces "one" when (_ == 1)
    2 produces "two" when (_ == 2)
    3 produces "three" when (_ == 3)
  }

  val e2 = new Engine[Int, String]("e2") {
    1 produces "onetwo" by (i => e(i) + e(i + 1))
    2 produces "twothree"
  }
  val cn_e_1 = e.decisionTree.conclusionNodeFor(e, 1)
  val cn_e_2 = e.decisionTree.conclusionNodeFor(e, 2)
  val cn_e_3 = e.decisionTree.conclusionNodeFor(e, 3)

  val cn_e2 = e2.decisionTree.conclusionNodeFor(e2, 1)

  "An engine" should "produce a trace" in {

    val (result, items) = Engine.trace(e, 1)
    result shouldBe "one"
    items shouldBe List(TraceEngine(0, 0, e, cn_e_1, 1, "one", List()))
  }

  it should "produces a nested trace" in {
    val (result, items) = Engine.trace(e2, 1)
    result shouldBe "onetwo"
    items.last shouldBe
      TraceEngine(0, 0, e2, cn_e2, 1, "onetwo", List(
        TraceEngine(0, 0, e, cn_e_1, 1, "one", List()),
        TraceEngine(0, 0, e, cn_e_2, 2, "two", List())
      )

      )

  }
}
