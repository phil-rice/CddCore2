package org.cddcore.examples

import org.cddcore.core.engine.CddSpec

class TennisSpec extends CddSpec {

  val tennis = new Tennis().tennis
  "The tennis engine" should "build" in {
    tennis(0, 0) shouldBe "love all"
    tennis(0, 1) shouldBe "love, fifteen"
    tennis(0, 2) shouldBe "love, thirty"
    tennis(1, 2) shouldBe "fifteen, thirty"
    tennis(1, 3) shouldBe "fifteen, forty"
    tennis(1, 4) shouldBe "receiver won"
    tennis(4, 1) shouldBe "server won"
    tennis.validate
  }
}