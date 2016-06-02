package org.cddcore.json

import org.cddcore.structure.PathRoot
import org.cddcore.utilities.{CddSpec, Display, DisplayProcessor, DontDisplay}
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

  it should "have a nice html display string" in {
    val dp = DisplayProcessor()
    val html = dp.html(JsonForTest)
    html shouldBe ""
  }
  it should "have a nice summary display string" in {
    val dp = DisplayProcessor()
    val summary: String = dp.summary(JsonForTest)
    summary shouldBe ""
  }
  it should "have a nice detailed display string" in {
    val dp = DisplayProcessor()
    val detailed: String = dp.detailed(JsonForTest)
    detailed shouldBe ""
  }
}
