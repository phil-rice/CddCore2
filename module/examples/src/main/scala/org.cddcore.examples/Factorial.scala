package org.cddcore.examples

import org.cddcore.engine.Engine


object Factorial {
  val factorial = new Engine[Int, Int] ("Factorial"){
    1 produces 1
    2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
  }
}
