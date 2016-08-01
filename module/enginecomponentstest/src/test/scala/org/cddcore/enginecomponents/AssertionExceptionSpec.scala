package org.cddcore.enginecomponents

import com.sun.xml.internal.ws.policy.AssertionSet
import org.cddcore.utilities.{CddSpec, NullLifeCycle, Strings}

class AssertionExceptionSpec extends CddSpec {

  import Scenario._

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  val oneProducesOne = "1" produces "1"

  "Assertions exceptions" should "handles EqualsAssertions not matching" in {
    val assertion = new EqualsAssertion[String, String]("someString")
    val exception = AssertionInvalidException(oneProducesOne, assertion, "someOtherString")
    Strings.cleanString(exception.getMessage) shouldBe Strings.cleanString(
      """The scenario defined at (AssertionExceptionSpec.scala:12) does not come to the result it is supposed to
        |Expected result: someString
        |Actual result :  someOtherString]""".stripMargin)
    exception.actual shouldBe "someOtherString"
    exception.scenario shouldBe oneProducesOne
  }

}
