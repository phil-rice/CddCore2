package org.cddcore.examples

import org.cddcore.utilities.CddSpec


class FibonacciSpec extends CddSpec {

  import Fibonacci._

  "The fibonacci" should "calculate a series" in {
    fibonacci(1) shouldBe 1
    fibonacci(2) shouldBe 2
    fibonacci(3) shouldBe 3
    fibonacci(4) shouldBe 5
    fibonacci(5) shouldBe 8
  }

}
