package org.cddcore.enginecomponents

import org.cddcore.utilities.CddSpec

class OrScenarioReasonSpec extends CddSpec {

  "A OrScenarioReason" should "have a pretty description that is the 'or' of the reasons in it" in {
    def reason(description: String) = {
      val r = mock[ScenarioReason[Int, String]]
      r.prettyDescription returns description
      r
    }
    OrScenarioReason(List(reason("one"), reason("two"), reason("three"))).prettyDescription shouldBe "one or two or three"
  }

  it should "return true for isDefinedAt if any of the reasons have a true isDefined at" in {
    val p = 1
    val engine = mock[Int => String]
    def reason(defined: Boolean) = {
      val r = mock[ScenarioReason[Int, String]]
      r.isDefinedAt(engine, p) returns defined
      r
    }
    OrScenarioReason(List(reason(false), reason(false), reason(false))).isDefinedAt(engine, p) shouldBe false
    OrScenarioReason(List(reason(false), reason(false), reason(true))).isDefinedAt(engine, p) shouldBe true
    OrScenarioReason(List(reason(false), reason(true), reason(false))).isDefinedAt(engine, p) shouldBe true
    OrScenarioReason(List(reason(true), reason(false), reason(false))).isDefinedAt(engine, p) shouldBe true
    OrScenarioReason(List(reason(true), reason(true), reason(true))).isDefinedAt(engine, p) shouldBe true
  }

  it should "call apply on the first reason in the list that 'isDefinedAt' is true for" in {
    val p = 1
    val result = "Result"
    val engine = mock[Int => String]

    def reason(defined: Boolean, setUp: Boolean = true) = {
      val r = mock[ScenarioReason[Int, String]]
      if (setUp) {
        r.isDefinedAt(engine, p) returns defined
        if (defined)
          r.apply(engine, p) returns result
      }
      r
    }
    OrScenarioReason(List(reason(false), reason(false), reason(true))).apply(engine, p) shouldBe result
    OrScenarioReason(List(reason(false), reason(true), reason(false))).apply(engine, p) shouldBe result
    OrScenarioReason(List(reason(true), reason(false), reason(false))).apply(engine, p) shouldBe result
    OrScenarioReason(List(reason(true), reason(true, setUp = false), reason(true, setUp = false))).apply(engine, p) shouldBe result
  }

  it should "throw an exception if not defined when apply is called " in {
    val p = 1
    val result = "Result"
    val engine = mock[Int => String]

    def reason(defined: Boolean, setUp: Boolean = true) = {
      val r = mock[ScenarioReason[Int, String]]
      if (setUp) {
        r.isDefinedAt(engine, p) returns defined
        if (defined)
          r.apply(engine, p) returns result
      }
      r
    }
    intercept[EngineIsNotDefined](OrScenarioReason(List(reason(false), reason(false), reason(false))).apply(engine, p))
  }


}
