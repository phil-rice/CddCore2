/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineIsNotDefined, Scenario}
import org.cddcore.utilities.{CddSpec, NullLifeCycle}

class ConclusionNodeSpec extends CddSpec {

  import Scenario._

  implicit def lifeCycle[C] = new NullLifeCycle[C]

  val mockEngine = (s: String) => 1
  "A conclusion node" should "have is defined at as the aggregate of the scenarios" in {
    val s1 = "abc" produces 1 when (_ contains "a")
    val s2 = "ab" produces 2 when (_ contains "b")

    val cn = ConclusionNode(s1, List(s2))

    cn.isDefinedAt(mockEngine, "a") shouldBe true
    cn.isDefinedAt(mockEngine, "b") shouldBe true
    cn.isDefinedAt(mockEngine, "c") shouldBe false
  }

  it should "apply the suitable scenario" in {
    val s1 = "abc" produces 1 when (_ contains "a")
    val s2 = "ab" produces 2 when (_ contains "b")

    val cn = ConclusionNode(s1, List(s2))

    cn(mockEngine, "a") shouldBe 1
    cn(mockEngine, "b") shouldBe 2
    val e = intercept[EngineIsNotDefined](cn(mockEngine, "c"))
  }
}
