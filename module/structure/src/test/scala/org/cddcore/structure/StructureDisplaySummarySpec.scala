package org.cddcore.structure

import org.cddcore.utilities.{CddSpec, DisplayProcessor, Strings}

class StructureDisplaySummarySpec extends CddSpec with XmlTestMother {

  val dp = DisplayProcessor()

  "An Xml Situation with zero fragments" should "produce a decent summary, which is first  hundred characters of the toString" in {

    val fragment: XmlZeroFragment = new XmlZeroFragment(x)
    dp.summary(fragment) shouldBe  fragment.toString.take(100)
  }
  "An Xml Situation with one fragments" should "produce a summary which is the first variable, if there is no @summary" in {
    dp.summary(new XmlOneFragment(x)) shouldBe "XmlOneFragment(one -> 1)"
    dp.summary(new XmlThreeFragment(x)) shouldBe "XmlThreeFragment(one -> 1)"
  }
  "An Xml Situation with one fragments" should "produce a summary which is the aggregate of the @summary fields" in {
    dp.summary(new XmlThreeFragmentWithSummaries(x)) shouldBe "XmlThreeFragmentWithSummaries(two -> 2,three -> 123)"
  }


  "An Xml Situation" should "report No Convertor when not configured properly for summary" in {
    dp.summary(new XmlWithJustRoot(x)) shouldBe "XmlWithJustRoot(one -> No Convertor)"
    dp.summary(new XmlWithJustRootAndStep(x)) shouldBe "XmlWithJustRootAndStep(one -> No Convertor)"
  }
//
  "An Xml Situation" should "handle the Elem not being there for summary" in {
    dp.summary(new XmlWithoutVariable(x)) shouldBe "XmlWithoutVariable(notIn -> None)"
  }

  "An Xml Situation with a fragment that isn't present" should "produce a decent summary" in {
    dp.summary(new XmlOneFragmentNotFound(x)) shouldBe "XmlOneFragmentNotFound(notIn -> )"
  }
  "A path result that throws an exception" should "still report that in the summary" in {
    dp.summary(new XmlWithException()) shouldBe "XmlWithException(path -> <error evaluating path>RuntimeException/some message)"
  }
}
