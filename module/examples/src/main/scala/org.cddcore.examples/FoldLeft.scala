package org.cddcore.examples

import org.cddcore.engine.FoldLeftEngine


object FoldLeft {
  val engine = new FoldLeftEngine[Int, Int] {
    (0, 0) produces 0 because { case (acc, v) => acc + v }
    (3, 2) produces 5
  }

  def main(args: Array[String]) {
    engine.validate
    println(List(1, 2, 3).foldLeft(0)(engine))
  }
}
