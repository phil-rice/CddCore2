package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, DisplayProcessor}


class ScenarioAssertionSpec extends CddSpec {


  implicit val displayProcessor = DisplayProcessor.defaultDisplayProcessor.
    withDetailer { case (dp, s: String) => s"(($s))" }.
    withSummarize { case (dp, s: String) => s"[[$s]]" }.
    withHtml { case (dp, s: String) => s"{{$s}}" }

  "An Equals Assertion" should "display the prettyExpected using the display processor" in {
    val assertionDp = new EqualsAssertion[String, String]("someString").expectedDp
    assertionDp.map(_.detailed) shouldBe Some("((someString))")
    assertionDp.map(_.summary) shouldBe Some("[[someString]]")
    assertionDp.map(_.html) shouldBe Some("{{someString}}")

  }

  "A result Assertion" should "display None as the prettyExpected " in {
    new ResultAssertion[String, String](_ => true).expectedDp shouldBe None
  }
  "An unknown Assertion" should "display None as the prettyExpected" in {
    new UnknownAssertion[String, Int]().expectedDp shouldBe None

  }
}
