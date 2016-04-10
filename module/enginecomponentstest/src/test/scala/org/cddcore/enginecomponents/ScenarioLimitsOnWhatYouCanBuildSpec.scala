/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, NullLifeCycle}

class ScenarioLimitsOnWhatYouCanBuildSpec extends CddSpec {

  import Scenario._

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  val oneProducesOne = 1 produces "1"
  val oneProducesOneMessage = "Scenario defined at (ScenarioLimitsOnWhatYouCanBuildSpec.scala:11)"

  val oneProducesOneWhere = oneProducesOne where (_ == 1)
  val oneProducesOneBy = oneProducesOne by (_.toString)
  val oneProducesOneWhen = oneProducesOne when (_ == 1)
  val oneProducesOneWhenBy = oneProducesOne when (_ == 1) by (_.toString)
  val oneProducesOneBecause = oneProducesOne because { case x => x.toString }

  "oneProducesOneBy" should "not allow because" in {
    intercept[ScenarioCannotHaveWhenByAndBecauseException] {
      oneProducesOneBy because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second by" in {
    intercept[ScenarioCannotHaveWhenByAndBecauseException] {
      oneProducesOneBy because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  "oneProducesOneWhen" should "not allow because" in {
    intercept[ScenarioCannotHaveWhenByAndBecauseException] {
      oneProducesOneWhen because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second when" in {
    intercept[ScenarioCannotHaveSecondWhenException] {
      oneProducesOneWhen when (_ == 1)
    }.getMessage shouldBe oneProducesOneMessage
  }

  "oneProducesOneWhenBy" should "not allow because" in {
    intercept[ScenarioCannotHaveWhenByAndBecauseException] {
      oneProducesOneWhenBy because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second when" in {
    intercept[ScenarioCannotHaveSecondWhenException] {
      oneProducesOneWhenBy when (_ == 1)
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second by" in {
    intercept[ScenarioCannotHaveSecondByException] {
      oneProducesOneWhenBy by (_.toString)
    }.getMessage shouldBe oneProducesOneMessage
  }

  "oneProducesOneBecause" should "not allow by" in {
    intercept[ScenarioCannotHaveWhenByAndBecauseException] {
      oneProducesOneBecause by { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second because" in {
    intercept[ScenarioCannotHaveSecondBecauseException] {
      oneProducesOneBecause because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }


}
