package org.cddcore.structure

import org.cddcore.utilities.{CddSpec, Strings}

class XmlToStringSpec extends CddSpec with XmlTestMother {

  "An Xml Situation with zero fragments" should "produce a decent toString" in {
    val actual = new XmlZeroFragment(x).toString
    val expected =
      s"""XmlZeroFragment(
          |xml
          |  x -> $xOneLine)""".stripMargin
    //Strings.bruteForceCompare(actual, expected)
    actual shouldBe expected

  }
  "An Xml Situation with one fragments" should "produce a decent toString" in {
    new XmlOneFragment(x).toString shouldBe
      s"""XmlOneFragment(
          |  one = 1
          |xml
          |  x -> $xOneLine)""".stripMargin
  }

//  "An Xml Situation" should "report No Convertor when not configured properly" in {
//    new XmlWithoutType(x).toString shouldBe
//      """XmlWithoutType(
//        "  one = No Convertor
//        |  Xml: x
//        |    $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation" should "handle the Elem not being a variable" in {
//    new XmlWithoutVariable().toString shouldBe
//      """XmlWithoutVariable(
//        |notIn = absent
//        |  Xml: x
//        |    $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation with simple repeating blocks and a fold" should "produce a decent toString" in {
//    new XmlThreeFragment(x).toString shouldBe
//      """XmlThreeFragment(
//        |  one = 1
//        |  two = 2
//        |  repeatedString = 123
//        |  repeatedInteger = 123
//        |  repeatedFolded = 6
//        |  Xml: x
//        |    $xOneLine)""".stripMargin
//  }
//
//
//
//  "An Xml Situation with nested blocks" should "produce a decent toString" in {
//    new XmlRepeatingFragments().toString shouldBe
//      """XmlRepeatingFragments(\n" +
//        |  repeatedFold = NumberFormatException For input string: ""
//        |  repeatedInteger = 1234
//        |  repeatedNestedFold = 10
//        |  repeatedNestedList = List(1, 2, 3, 4)
//        |  repeatedNestedString = 1234
//        |  repeatedString = 1234
//        |  Xml: x
//        |    $xOneLine)""".stripMargin
//  }
//
//  "An Xml Situation with a fragment that isn't present" should "produce a decent toString" in {
//    new XmlOneFragmentNotFound(x).toString shouldBe
//      """XmlOneFragmentNotFound(
//        |  notIn =
//        |  Xml: x
//        |    $xOneLine)""".stripMargin
//  }
}
