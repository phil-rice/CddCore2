package org.cddcore.examples

import org.cddcore.utilities.CddSpec


class FactorialSpec extends CddSpec{
import Factorial._
  "The factorial engine" should "produce factorials" in {
    factorial(1) shouldBe 1
    factorial(2) shouldBe 2
    factorial(3) shouldBe 6
    factorial(4) shouldBe 24
  }
}
