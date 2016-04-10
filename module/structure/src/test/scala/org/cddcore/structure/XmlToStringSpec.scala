/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
          |  notIn -> !
          |xml
          |  x -> $xOneLine)""".stripMargin.replace("!", "")

  }
  "A path result that throws an exception" should "still report that in the toString" in {
    new XmlWithException().toString shouldBe
      s"""XmlWithException(
          |  path -> <error evaluating path>RuntimeException/some message
          |xml
          |  x -> <some><item>1</item></some>)""".stripMargin
  }
}
