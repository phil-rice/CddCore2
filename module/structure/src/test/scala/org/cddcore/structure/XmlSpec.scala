package org.cddcore.structure

import org.cddcore.utilities.CddSpec

import scala.xml.Node


class XmlSpec extends CddSpec with XmlTestMother {

  class XmlHolderForTest(val n: Node) extends Xml

  "An Xml situation holder " should "allow simple ints and strings to be represented" in {
    val situation = new XmlHolderForTest(
      <root>
        <one>1</one> <two>2</two>
      </root>) {
      val oneI = xml(n)\ "one" \ int
      val oneS = xml(n)\ "one" \ string
    }
    situation.oneI() shouldBe 1
    situation.oneS() shouldBe "1"

  }

//  "An Xml Situation holder with nested blocks" should "evaluate fragments" in {
//    val situation = new XmlRepeatingFragments
//    situation.repeatedString() shouldBe "1234"
//    situation.repeatedNestedString() shouldBe "1234"
//    situation.repeatedInteger() shouldBe 1234
//    situation.repeatedNestedFold() shouldBe 10
//    situation.repeatedNestedList() shouldBe List(1, 2, 3, 4)
//    intercept[NumberFormatException] {
//      situation.repeatedFold()
//    }
//  }
}





