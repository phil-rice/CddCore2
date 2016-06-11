/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
