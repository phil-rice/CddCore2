/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.CddSpec


class ScenarioBuilderAndLifeCycleSpec extends CddSpec {
  type S = Scenario[Int, String]

  import Scenario._

  "A scenario builder" should "tell the implicit life cycle when a scenario is created" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = 2 produces "two"
    val s3 = 3 produces "three"
    lc.created shouldBe List(s1, s2, s3)
    lc.modified shouldBe List()
    lc.errorStrings shouldBe List()
  }

  it should "tell the implicit life cycle when a scenario is modified using when" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 when (_ == 1)
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/WhenByReason")
    lc.errorStrings shouldBe List()
  }
  it should "tell the implicit life cycle when a scenario is modified using by" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 by (_ => "one")
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/WhenByReason")
    lc.errorStrings shouldBe List()
  }
  it should "tell the implicit life cycle when a scenario is modified using because" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = (1 produces "one")
    val s2 = s1 because { case _ => "one" }
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/BecauseReason")
    lc.errorStrings shouldBe List()
  }

  it should "do evaluate the changes to the scenario inside the ChildLifeCycle update method, which means the exceptions happen in a managed way" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 when (_ == 2)
    lc.created shouldBe List(s1)
    lc.modified shouldBe List()
    lc.errorStrings shouldBe List("ReasonInvalidException/Scenario defined at (ScenarioBuilderAndLifeCycleSpec.scala:49) cannot be added because the reason given isn't valid")
  }
}
