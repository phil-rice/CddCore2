/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
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
    new DecisionTreeBuilder(mockEngine).addOne( dn, s1) shouldBe (mainS ifTrue(mainS, s1) ifFalse falseS)
    new DecisionTreeBuilder(mockEngine).addOne( dn, s2) shouldBe (mainS ifTrue mainS ifFalse(falseS, s2))
  }

}

