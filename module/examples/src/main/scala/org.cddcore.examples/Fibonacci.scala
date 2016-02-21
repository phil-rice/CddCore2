package org.cddcore.examples

import org.cddcore.engine.Engine

object Fibonacci {

  val fibonacci = new Engine[Int, Int] {
    1 produces 1 when (_ == 1)
    2 produces 2 when (_ == 2)
    3 produces 3 byRecursion { case (engine, i) => engine(i - 1) + engine(i - 2) }
  }
}
