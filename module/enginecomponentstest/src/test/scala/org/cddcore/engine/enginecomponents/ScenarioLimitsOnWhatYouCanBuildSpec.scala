package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CddSpec

class ScenarioLimitsOnWhatYouCanBuildSpec extends CddSpec {

  import Scenario._

  val oneProducesOne = 1 produces "1"
  val oneProducesOneMessage = "Scenario defined at (ScenarioLimitsOnWhatYouCanBuildSpec.scala:9)"

  val oneProducesOneWhere = oneProducesOne where (_ == 1)
  val oneProducesOneBy = oneProducesOne by (_.toString)
  val oneProducesOneWhen = oneProducesOne when (_ == 1)
  val oneProducesOneWhenBy = oneProducesOne when (_ == 1) by (_.toString)
  val oneProducesOneBecause = oneProducesOne because { case x => x.toString }

  "oneProducesOneBy" should "not allow because" in {
    intercept[ScenarioCannotHaveByAndBecauseException] {
      oneProducesOneBy because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second by" in {
    intercept[ScenarioCannotHaveByAndBecauseException] {
      oneProducesOneBy because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  "oneProducesOneWhen" should "not allow because" in {
    intercept[ScenarioCannotHaveWhenAndBecauseException] {
      oneProducesOneWhen because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second when" in {
    intercept[ScenarioCannotHaveSecondWhenException] {
      oneProducesOneWhen when (_ == 1)
    }.getMessage shouldBe oneProducesOneMessage
  }

  "oneProducesOneWhenBy" should "not allow because" in {
    intercept[ScenarioCannotHaveWhenAndBecauseException] {
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
    intercept[ScenarioCannotHaveByAndBecauseException] {
      oneProducesOneBecause by { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }

  it should "not allow second because" in {
    intercept[ScenarioCannotHaveSecondBecauseException] {
      oneProducesOneBecause because { case x => x.toString }
    }.getMessage shouldBe oneProducesOneMessage
  }


}
