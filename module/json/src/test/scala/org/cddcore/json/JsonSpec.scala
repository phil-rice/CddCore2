/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.json

import org.cddcore.structure.PathRoot
import org.cddcore.utilities._
import org.json4s.JValue

class JsonSpec extends CddSpec {

  import JsonSituation._

  object JsonForTest extends JsonSituation {
    @DontDisplay
    val json: PathRoot[JValue] = parse("""{"a": 1, "b": 2, "c": {"d": 1}}""")
    val aAsString = json \ "a" \ string
    val aAsInt = json \ "a" \ int
    val b = json \ "b" \ int
    val cd = json \ "c" \ "d" \ int
    @Display
    val someVariable = "someValue"
    val someHiddenVariable = "someHiddenValue"
  }

  "Json structures" should "allow easy access to data in the structure" in {
    JsonForTest.aAsInt() shouldBe 1
    JsonForTest.aAsString() shouldBe "1"
    JsonForTest.b() shouldBe 2
    JsonForTest.cd() shouldBe 1
  }

  it should "have a nice summary display string" in {
    val dp = DisplayProcessor()
    val summary: String = dp.summary(JsonForTest)
    summary shouldBe "JsonForTest$(someVariable -> someValue)"
  }
  it should "have a nice detailed display string" in {
    val dp = DisplayProcessor()
    Strings.splitLines(dp.detailed(JsonForTest)) shouldBe Strings.splitLines(
      """JsonForTest$(
        |  aAsString -> 1
        |  aAsInt -> 1
        |  b -> 2
        |  cd -> 1
        |  someVariable -> someValue
        |)""".stripMargin)
  }
}
