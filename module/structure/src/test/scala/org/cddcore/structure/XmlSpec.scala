package org.cddcore.structure

import org.cddcore.utilities.CddSpec

class XmlSpec extends CddSpec with XmlTestMother {
  "An Xml Situation with nested blocks" should "evaluate fragments" in {
    val situation = new XmlRepeatingFragments
    situation.repeatedString() shouldBe "1234"
    situation.repeatedNestedString() shouldBe "1234"
    situation.repeatedInteger() shouldBe 1234
    situation.repeatedNestedFold() shouldBe 10
    situation.repeatedNestedList() shouldBe List(1, 2, 3, 4)
    intercept[NumberFormatException] {
      situation.repeatedFold()
    }
  }
}





