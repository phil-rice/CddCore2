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
          |  one -> 1
          |xml
          |  x -> $xOneLine)""".stripMargin
  }

  "An Xml Situation" should "report No Convertor when not configured properly" in {
    new XmlWithJustRoot(x).toString shouldBe
      s"""XmlWithJustRoot(
          |  one -> No Convertor
          |xml
          |  x -> $xOneLine)""".stripMargin
    new XmlWithJustRootAndStep(x).toString shouldBe
      s"""XmlWithJustRootAndStep(
          |  one -> No Convertor
          |xml
          |  x -> $xOneLine)""".stripMargin
  }

  "An Xml Situation" should "handle the Elem not being there" in {
    new XmlWithoutVariable(x).toString shouldBe
      s"""XmlWithoutVariable(
          |  notIn -> None
          |xml
          |  x -> $xOneLine)""".stripMargin
  }

  "An Xml Situation with simple repeating blocks and a fold" should "produce a decent toString" in {
    new XmlThreeFragment(x).toString shouldBe
      s"""XmlThreeFragment(
          |  one -> 1
          |  two -> 2
          |  repeatedString -> 123
          |  repeatedInteger -> 123
          |  repeatedFolded -> 6
          |xml
          |  x -> $xOneLine)""".stripMargin
  }

  "An Xml Situation with nested blocks" should "produce a decent toString" in {
    val situation = new XmlRepeatingNestedFragments()
    situation.toString shouldBe
      s"""XmlRepeatingNestedFragments(
          |  repeatedString -> 1234
          |  repeatedNestedString -> 1234
          |  repeatedNestedFold -> 10
          |  repeatedNestedList -> List(1, 2, 3, 4)
          |xml
          |  x -> ${Strings.oneLine(situation.x)})""".stripMargin
  }

  "An Xml Situation with a fragment that isn't present" should "produce a decent toString" in {
    new XmlOneFragmentNotFound(x).toString shouldBe
      s"""XmlOneFragmentNotFound(
          |  notIn -> 
          |xml
          |  x -> $xOneLine)""".stripMargin

  }
}
