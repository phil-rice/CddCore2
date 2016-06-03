/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.structure

import org.cddcore.utilities.{Strings, Summary}

import scala.xml.{Elem, Node}

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

  class XmlZeroFragment(val x: Elem) extends Situation[Node] {
  }

  class XmlOneFragment(val x: Elem) extends Situation[Node] {
    val one = root(x) \ "one" \ string
  }

  class XmlWithJustRoot(val x: Elem) extends Situation[Node] {
    val one = root(x)
  }

  class XmlWithJustRootAndStep(val x: Elem) extends Situation[Node] {
    val one = root(x) \ "one"
  }

  class XmlOneFragmentNotFound(val x: Elem) extends Situation[Node] {
    val notIn = root(x) \ "absent" \ string
  }

  class XmlWithoutVariable(val x: Elem) extends Situation[Node] {
    val notIn = root(x) \ "absent" \ optString
  }

  class XmlThreeFragment(val x: Elem) extends Situation[Node]() {
    val one = root(x) \ "one" \ int
    val two = root(x) \ "two" \ string
    val repeatedString = root(x) \ "repeated" \ string
    val repeatedInteger = root(x) \ "repeated" \ int
    val repeatedFolded = root(x) \ "repeated" \ Fold.int(0)((l, r) => l + r)
  }

  class XmlThreeFragmentWithSummaries(val x: Elem) extends Situation[Node]() {
    val one = root(x) \ "one" \ int
    @Summary
    val two = root(x) \ "two" \ string
    @Summary
    val three = root(x) \ "repeated" \ string
  }

  class XmlRepeatingNestedFragments extends Situation[Node] {
    val x = <root>
      <repeated>
        <nested>1</nested> <nested>2</nested>
      </repeated>
      <repeated>
        <nested>3</nested> <nested>4</nested>
      </repeated>
      <repeated></repeated>
    </root>

    val repeatedString = root(x) \ "repeated" \ "nested" \ string
    val repeatedNestedString = root(x) \ "repeated" \ "nested" \ string
    val repeatedNestedFold = root(x) \ "repeated" \ "nested" \ Fold.int(0)((l, r) => l + r)
    val repeatedNestedList = root(x) \\ "nested" \ Fold.list[Int](_.text.toInt)
  }

  class XmlWithException extends Situation[Node] {
    val e = new RuntimeException("some message")
    val exceptionPathResult = customPathResult[String, Int](_ => throw e, AggregateStrings)
    val x = <some><item>1</item></some>
    val path = root(x) \ "item" \ exceptionPathResult
  }

}
