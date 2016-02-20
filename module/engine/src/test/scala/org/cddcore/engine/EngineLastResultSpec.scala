package org.cddcore.engine

import org.cddcore.engine.enginecomponents.Scenario
import org.cddcore.utilities.CddSpec

class EngineLastResultSpec extends CddSpec {

  "last" should "be the result of the previous scenario in the same use case" in {
    var remembered1 = ""
    var remembered77 = ""

    new Engine[Int, String] {
      useCase("1") {
        1 produces "1"
        remembered1 = last
      }
      useCase("1") {
        77 produces "77"
        remembered77 = last
      }
    }
    remembered1 shouldBe "1"
    remembered77 shouldBe "77"
  }

  it should "be usable in a scenario" in {
    var remembered: Option[Scenario[String, String]] = None
    new Engine[String, String] {
      useCase("1") {
        "0" produces "1"
        remembered = Some(last produces "2")
      }
    }
    remembered.map(_.situation) shouldBe Some("1")
  }

  it should "throw NoLastException if at the start of an engine" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      rememberedException = intercept[NoLastException](last)

    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if no result is available" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      1 produces something where (_ => true)
      rememberedException = intercept[NoLastException](last)
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if at the start of a use case" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      useCase("1") {
        rememberedException = intercept[NoLastException](last)
      }
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if at the start of a newuse case" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      useCase("0") {}
      useCase("1") {
        rememberedException = intercept[NoLastException](last)
      }
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }


}
