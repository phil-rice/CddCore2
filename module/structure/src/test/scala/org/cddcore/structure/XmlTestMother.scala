package org.cddcore.structure

import org.cddcore.utilities.Strings

import scala.xml.Elem

/**
  * Created by Phil on 23/03/2016.
  */
trait XmlTestMother {

  val x =
    <root>
      <one>1</one>
      <two>2</two>
      <repeated>1</repeated>
      <repeated>2</repeated>
      <repeated>3</repeated>
    </root>

  val xOneLine = Strings.oneLine(x.toString)

  class XmlZeroFragment(val x: Elem) extends Xml() {
  }

  class XmlOneFragment(val x: Elem) extends Xml() {
    val one = xml(x) \ "one" \ string
  }

  class XmlWithoutType(val x: Elem) extends Xml() {
    val one = xml(x) \ "one"
  }

  class XmlOneFragmentNotFound(x: Elem) extends Xml() {
    val notIn = xml(x) \ "absent" \ string
  }

  class XmlWithoutVariable extends Xml() {
    val notIn = xml(x) \ "absent" \ string
  }

  class XmlThreeFragment(val x: Elem) extends Xml() {
    val one = xml(x) \ "one" \ int
    val two = xml(x) \ "two" \ string
    val repeatedString = xml(x) \ "repeated" \ string
    val repeatedInteger = xml(x) \ "repeated" \ int
    val repeatedFolded = xml(x) \ "repeated" \ fold[Int, Int](int, 0, (l, r) => l + r)
  }

  class XmlRepeatingFragments extends Xml {
    val x = <root>
      <repeated>
        <nested>1</nested> <nested>2</nested>
      </repeated>
      <repeated>
        <nested>3</nested> <nested>4</nested>
      </repeated>
      <repeated></repeated>
    </root>

    val repeatedString = xml(x) \ "repeated" \ string
    val repeatedNestedString = xml(x) \ "repeated" \ string
    val repeatedInteger = xml(x) \ "repeated" \ int
    val repeatedFold = xml(x) \ "repeated" \ fold[Int, Int](int, 0, (l, r) => l + r)
    val repeatedNestedFold = xml(x) \ "repeated" \ "nested" \ fold[Int, Int](int, 0, (l, r) => l + r)
    val repeatedNestedList = xml(x) \ "repeated" \ "nested" \ fold[List[Int], Int](int, List(), (acc, v) => acc :+ v)
  }

}
