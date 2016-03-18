package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.{CddSpec, ChildLifeCycle}


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
    lc.errors shouldBe List()
  }

  it should "tell the implicit life cycle when a scenario is modified using when" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 when (_ == 1)
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/WhenByReason")
    lc.errors shouldBe List()
  }
  it should "tell the implicit life cycle when a scenario is modified using by" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 by (_ => "one")
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/WhenByReason")
    lc.errors shouldBe List()
  }
  it should "tell the implicit life cycle when a scenario is modified using because" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = (1 produces "one")
    val s2 = s1 because { case _ => "one" }
    lc.created shouldBe List(s1)
    lc.modified shouldBe List("1/EqualsAssertion(one)/BecauseReason")
    lc.errors shouldBe List()
  }

  it should "do evaluate the changes to the scenario inside the ChildLifeCycle update method, which means the exceptions happen in a managed way" in {
    implicit val lc = new RememberingLifeCycle[Int, String]
    val s1 = 1 produces "one"
    val s2 = s1 when (_ == 2)
    lc.created shouldBe List(s1)
    lc.modified shouldBe List()
    lc.errors shouldBe List("ReasonInvalidException/Scenario defined at (ScenarioBuilderAndLifeCycleSpec.scala:48) cannot be added because the reason given isn't actually true")
  }
}
