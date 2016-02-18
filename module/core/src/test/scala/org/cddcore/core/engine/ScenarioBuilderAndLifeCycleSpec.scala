package org.cddcore.core.engine

import org.cddcore.core.builder.ChildLifeCycle


class ScenarioBuilderAndLifeCycleSpec extends CddSpec {
  type S = Scenario[Int, String]

  class RememberingLifeCycle extends ChildLifeCycle[S] {
    var created = List[S]()
    var modified = List[String]()

    def asString(s: S) = s.situation + "/" + s.expected + "/" + s.reason.getClass.getSimpleName

    def created(child: S) = created = created :+ child

    def modified(oldChild: S, newChild: S) = modified = modified :+ (asString(oldChild) + "==>" + asString(newChild))
  }

  import Scenario._

  "A scenario builder" should "tell the implicit life cycle when a scenario is created" in {
    implicit val lc = new RememberingLifeCycle
    val s1 = 1 produces "one"
    val s2 = 2 produces "two"
    val s3 = 3 produces "three"
    lc.created shouldBe List(s1, s2, s3)
  }

  it should "tell the implicit life cycle when a scenario is modified using when" in {
    implicit val lc = new RememberingLifeCycle
    val s1 = 1 produces "one" when (_ == 1)
    lc.modified shouldBe List("1/one/SimpleReason==>1/one/WhenReason")
  }
  it should "tell the implicit life cycle when a scenario is modified using because" in {
    implicit val lc = new RememberingLifeCycle
    val s1 = 1 produces "one" because { case _ => "one" }
    lc.modified shouldBe List("1/one/SimpleReason==>1/one/BecauseReason")
  }
}
