package org.cddcore.structure

import org.cddcore.utilities.{CddSpec, DisplayProcessor, Strings}

class StructureDisplaySummarySpec extends CddSpec with XmlTestMother {

  val dp = DisplayProcessor()

  "An Xml Situation with zero fragments" should "produce a decent summary, which is first  hundred characters of the toString" in {

    val fragment: XmlZeroFragment = new XmlZeroFragment(x)
    dp.summary(fragment) shouldBe  fragment.toString.take(100)
  }
  "An Xml Situation with one fragments" should "produce a summary which is the first variable, if there is no @summary" in {
    dp.summary(new XmlOneFragment(x)) shouldBe "one -> 1"
    dp.summary(new XmlThreeFragment(x)) shouldBe "one -> 1"
  }
  "An Xml Situation with one fragments" should "produce a summary which is the aggregate of the @summary fields" in {
    dp.summary(new XmlThreeFragmentWithSummaries(x)) shouldBe "two -> 2, three -> 123"
  }


//  "An Xml Situation" should "report No Convertor when not configured properly for summary" in {
//    dp.summary(new XmlWithJustRoot(x)) shouldBe
//      s"""XmlWithJustRoot(
//          |  one -> No Convertor
//          |xml
//          |  x -> $xOneLine)""".stripMargin
//    dp.summary(new XmlWithJustRootAndStep(x)) shouldBe
//      s"""XmlWithJustRootAndStep(
//          |  one -> No Convertor
//          |xml
//          |  x -> $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation" should "handle the Elem not being there" in {
//    dp.summary(new XmlWithoutVariable(x)) shouldBe
//      s"""XmlWithoutVariable(
//          |  notIn -> None
//          |xml
//          |  x -> $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation with simple repeating blocks and a fold" should "produce a decent summary" in {
//    dp.summary(new XmlThreeFragment(x).toString) shouldBe
//      s"""XmlThreeFragment(
//          |  one -> 1
//          |  two -> 2
//          |  repeatedString -> 123
//          |  repeatedInteger -> 123
//          |  repeatedFolded -> 6
//          |xml
//          |  x -> $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation with nested blocks" should "produce a decent summary" in {
//    val situation = new XmlRepeatingNestedFragments()
//    dp.summary(situation) shouldBe
//      s"""XmlRepeatingNestedFragments(
//          |  repeatedString -> 1234
//          |  repeatedNestedString -> 1234
//          |  repeatedNestedFold -> 10
//          |  repeatedNestedList -> List(1, 2, 3, 4)
//          |xml
//          |  x -> ${Strings.oneLine(situation.x)})""".stripMargin
//  }
//
//  "An Xml Situation with a fragment that isn't present" should "produce a decent summary" in {
//    dp.summary(new XmlOneFragmentNotFound(x)) shouldBe
//      s"""XmlOneFragmentNotFound(
//          |  notIn -> !
//          |xml
//          |  x -> $xOneLine)""".stripMargin.replace("!", "")
//
//  }
//  "A path result that throws an exception" should "still report that in the summary" in {
//    dp.summary(new XmlWithException()) shouldBe
//      s"""XmlWithException(
//          |  path -> <error evaluating path>RuntimeException/some message
//          |xml
//          |  x -> <some><item>1</item></some>)""".stripMargin
//  }
}
