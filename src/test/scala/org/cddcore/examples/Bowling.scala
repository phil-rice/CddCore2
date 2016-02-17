package org.cddcore.examples

import org.cddcore.engine.{CddSpec, Engine}


class BowlingSpec extends CddSpec {

  sealed abstract class Frame(val first: Int, val second: Int, val third: Int = 0, val size: Int = 2) {
    def score = first + second + third;
  }

  case class NormalFrame(f: Int, s: Int) extends Frame(f, s);

  case class SpareFrame(f: Int, s: Int, t: Int) extends Frame(f, s, t);

  case class StrikeFrame(f: Int, s: Int, t: Int) extends Frame(f, s, t, 1);

  //get returns the ith ball or zero
  val get = new Engine[(List[Int], Int), Int]("Get") {
    useCase("If index in range of list (i.e. the ball has been rolled, return ith item")(
      (List(7, 10, 4, 3), 0) produces 7 because { case (rolls, i) if i >= 0 && i < rolls.size => rolls(i) }, //, "Start of legal range").expected(7).code((rolls: List[Int], i: Int) => rolls(i)).
      (List(7, 10, 4, 3), 3) produces 3 //, "End of legal range").expected(3).
    )
    useCase("If index is negative return zero")(
      (List(7, 10, 4, 3), -1) produces 0 when { case (_, i) => i < 0 }, //, "First value below legal range").expected(0).code((rolls: List[Any], i: Int) => 0).because((rolls: List[Any], i: Int) => i < 0).
      (List(7, 10, 4, 3), -100) produces 0 // "Quite a lot below legal range").expected(0).
    )
    useCase("If index is too high return zero")(
      (List(7, 10, 4, 3), 4) produces 0 when { case (rolls, i) => i >= rolls.size }, // "First value above legal range").expected(0).because((rolls: List[Any], i: Int) => i >= rolls.size).
      (List(7, 10, 4, 3), 100) produces 0 //, "Quite a lot above legal range").expected(0).build
    )
  }

  val makeFrame = new Engine[(List[Int], Int), Frame]("Bowling") {
    useCase("NormalFrames are produced when the two balls at and after the ith ball don't add up to 10")(
      (List(7, 2, 5, 5, 3, 0, 10, 2, 4), 0) produces NormalFrame(7, 2) because { case (rolls, i) => NormalFrame(get(rolls, i), get(rolls, i + 1)) },
      (List(7, 2, 5, 5, 3, 0, 10, 2, 4), 4) produces NormalFrame(3, 0)
    )

    useCase("Strike Frames are produced when the ith ball equals 10. They include the ith ball, and the next two balls")(
      (List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6) produces (StrikeFrame(10, 2, 4): Frame) because { case (rolls, i) if get(rolls, i) == 10 => StrikeFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)) },
      (List(10), 0) produces (StrikeFrame(10, 0, 0): Frame),
      (List(10, 10), 0) produces (StrikeFrame(10, 10, 0): Frame),
      (List(10, 10, 10), 0) produces (StrikeFrame(10, 10, 10): Frame)
    )

    useCase("Spare Frames are produced when the two balls at and after the ith ball add up to 10. They include the two balls, and the next ball")(
      (List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2) produces (SpareFrame(5, 5, 3): Frame) because { case (rolls, i) if get(rolls, i) + get(rolls, i + 1) == 10 => SpareFrame(get(rolls, i), get(rolls, i + 1), get(rolls, i + 2)) },
      (List(5, 5), 0) produces (SpareFrame(5, 5, 0): Frame)
    )
  }

  "it" should "do something" in {
    println(makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 6))
  }
}

//  def main(args: Array[String]) {
////    println(get.toString)
////    println(makeFrame.toString)
//    val (r, pr) = Engine.profile(makeFrame(List(7, 2, 5, 5, 3, 0, 10, 2, 4), 2))
//    println(r)
//    println(pr.prettyString)
//  }