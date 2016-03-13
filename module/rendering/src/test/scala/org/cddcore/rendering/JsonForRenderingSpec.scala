package org.cddcore.rendering

import org.cddcore.utilities.CddSpec

class JsonForRenderingSpec extends CddSpec{

  "The JsonForRenderering" should "pretty pretty a Map[String,Any" in {
    JsonForRendering.pretty(Map()) shouldBe "{ }"

    JsonForRendering.pretty(Map("a" -> 1)) shouldBe
      """{
        |  "a" : 1
        |}""".stripMargin
  }

}
