/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, NullLifeCycle}

class ScenarioToStringSpec extends CddSpec {

  import Scenario._

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  val oneProducesOne = 1 produces "1"
  "<situation> produces <result>" should "have a nice toString" in {
    oneProducesOne.toString shouldBe "Scenario(1 produces 1 JustBecause)/(ScenarioToStringSpec.scala:12)"
  }

  "<situation> produces <result> where <where>" should "have a nice toString" in {
    (oneProducesOne where (_ == 1)).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne where (x => x == 1)).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne where { case x => x == 1 }).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:12)"
  }

  "<situation> produces <result> by <by>" should "have a nice toString" in {
    (oneProducesOne by (_.toString)).toString shouldBe "Scenario(1 produces 1 by ((x$2: Int) => x$2.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne by (x => x.toString)).toString shouldBe "Scenario(1 produces 1 by ((x: Int) => x.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne by { case x => x.toString }).toString shouldBe "Scenario(1 produces 1 by case (x @ _) => x.toString())/(ScenarioToStringSpec.scala:12)"
  }

  "<situation> produces <result> where <where> by" should "have a nice toString" in {
    (oneProducesOne where (_ == "1") by (_.toString)).toString shouldBe "Scenario(1 produces something where <function1> by ((x$4: Int) => x$4.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne where (x => x == "1") by (x => x.toString)).toString shouldBe "Scenario(1 produces something where <function1> by ((x: Int) => x.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne where { case x => x == "1" } by { case x => x.toString }).toString shouldBe "Scenario(1 produces something where <function1> by case (x @ _) => x.toString())/(ScenarioToStringSpec.scala:12)"
  }


  "<situation> produces <result> because <because>" should "have a nice toString" in {
    (oneProducesOne because { case x => x.toString }).toString shouldBe "Scenario(1 produces 1 because {case (x @ _) => x.toString()})/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne because { case x if x == 1 => x.toString }).toString shouldBe "Scenario(1 produces 1 because {case (x @ _) if x.==(1) => x.toString()})/(ScenarioToStringSpec.scala:12)"
  }

  "<situation> produces <result> when <when>" should "have a nice toString" in {
    (oneProducesOne when (_ == 1)).toString shouldBe "Scenario(1 produces 1 when ((x$5: Int) => x$5.==(1)))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne when (x => x == 1)).toString shouldBe "Scenario(1 produces 1 when ((x: Int) => x.==(1)))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne when { case x => true }).toString shouldBe "Scenario(1 produces 1 when case (x @ _) => true)/(ScenarioToStringSpec.scala:12)"
  }

  "<situation> produces <result> when <when> by <by>" should "have a nice toString" in {
    (oneProducesOne when (_ == 1) by (_.toString)).toString shouldBe "Scenario(1 produces 1 when ((x$6: Int) => x$6.==(1)) by ((x$7: Int) => x$7.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne when (x => x == 1) by { x => x.toString }).toString shouldBe "Scenario(1 produces 1 when ((x: Int) => x.==(1)) by ((x: Int) => x.toString()))/(ScenarioToStringSpec.scala:12)"
    (oneProducesOne when { case x => true } by { case x: Int => x.toString }).toString shouldBe "Scenario(1 produces 1 when case (x @ _) => true by case (x @ (_: Int)) => x.toString())/(ScenarioToStringSpec.scala:12)"
  }
}
