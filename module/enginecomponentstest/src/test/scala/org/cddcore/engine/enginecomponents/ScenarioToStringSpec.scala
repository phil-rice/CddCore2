package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CddSpec

class ScenarioToStringSpec extends CddSpec {

  import Scenario._

  val oneProducesOne = 1 produces "1"
  "<situation> produces <result>" should "have a nice toString" in {
    oneProducesOne.toString shouldBe "Scenario(1 produces 1 JustBecause)/(ScenarioToStringSpec.scala:9)"
  }

  "<situation> produces <result> where <where>" should "have a nice toString" in {
    (oneProducesOne where (_ == 1)).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne where (x => x == 1)).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne where { case x => x == 1 }).toString shouldBe "Scenario(1 produces something where <function1> JustBecause)/(ScenarioToStringSpec.scala:9)"
  }

  "<situation> produces <result> by <by>" should "have a nice toString" in {
    (oneProducesOne by (_.toString)).toString shouldBe "Scenario(1 produces 1 SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne by (x => x.toString)).toString shouldBe "Scenario(1 produces 1 SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne by { case x => x.toString }).toString shouldBe "Scenario(1 produces 1 SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
  }

  "<situation> produces <result> where <where> by" should "have a nice toString" in {
    (oneProducesOne where (_ == 1) by (_.toString)).toString shouldBe "Scenario(1 produces something where <function1> SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne where (x => x == 1) by (x => x.toString)).toString shouldBe "Scenario(1 produces something where <function1> SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne where { case x => x == 1 } by { case x => x.toString }).toString shouldBe "Scenario(1 produces something where <function1> SimpleReasonWithBy())/(ScenarioToStringSpec.scala:9)"
  }


  "<situation> produces <result> because <because>" should "have a nice toString" in {
    (oneProducesOne because { case x => x.toString }).toString shouldBe "Scenario(1 produces 1 because {case (x @ _) => x.toString()})/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne because { case x if x == 1 => x.toString }).toString shouldBe "Scenario(1 produces 1 because {case (x @ _) if x.==(1) => x.toString()})/(ScenarioToStringSpec.scala:9)"
  }

  "<situation> produces <result> when <when>" should "have a nice toString" in {
    (oneProducesOne when (_ == 1)).toString shouldBe "Scenario(1 produces 1 when ((x$5: Int) => x$5.==(1)))/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne when (x => x == 1)).toString shouldBe "Scenario(1 produces 1 when ((x: Int) => x.==(1)))/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne when { case x => true }).toString shouldBe "Scenario(1 produces 1 when case (x @ _) => true)/(ScenarioToStringSpec.scala:9)"
  }

  "<situation> produces <result> when <when> by <by>" should "have a nice toString" in {
    (oneProducesOne when (_ == 1) by (_.toString)).toString shouldBe "Scenario(1 produces 1 when ((x$6: Int) => x$6.==(1)) by <function1>)/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne when (x => x == 1) by { x => x.toString }).toString shouldBe "Scenario(1 produces 1 when ((x: Int) => x.==(1)) by <function1>)/(ScenarioToStringSpec.scala:9)"
    (oneProducesOne when { case x => true } by { case x: Int => x.toString }).toString shouldBe "Scenario(1 produces 1 when case (x @ _) => true by <function1>)/(ScenarioToStringSpec.scala:9)"
  }
}
