package org.cddcore.examples

import org.cddcore.utilities.CddSpec

class BowlingSpec extends CddSpec {

  val makeFrame = new Bowling().makeFrame

  "it" should "do something" in {
    println(makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6))
    makeFrame.validate
  }
}

//  def main(args: Array[String]) {
////    println(get.toString)
////    println(makeFrame.toString)
//    val (r, pr) = Engine.profile(makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2))
//    println(r)
//    println(pr.prettyString)
//  }
