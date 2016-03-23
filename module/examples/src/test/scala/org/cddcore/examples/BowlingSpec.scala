package org.cddcore.examples

import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.utilities.CddSpec
import org.junit.runner.RunWith

class BowlingSpec extends CddSpec {

  import Bowling._

  "it" should "do something" in {
    makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6) shouldBe StrikeFrame(10, 2, 4)
    makeFrame.validate
  }
}


@RunWith(classOf[CddRunner])
class BowlingJUnit extends CddContinuousIntegrationTest {
  val engines = List(Bowling.get, Bowling.makeFrame)
}

//  def main(args: Array[String]) {
////    println(get.toString)
////    println(makeFrame.toString)
//    val (r, pr) = Engine.profile(makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2))
//    println(r)
//    println(pr.prettyString)
//  }
