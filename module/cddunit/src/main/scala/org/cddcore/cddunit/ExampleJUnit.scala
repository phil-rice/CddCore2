package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.junit.runner.{JUnitCore, RunWith}


@RunWith(classOf[CddRunner])
class ExampleJUnit extends CddContinuousIntegrationTest {

  val engine = new Engine[Int, String]("An engine") {
    useCase("a use case") {
      1 produces "one"
      2 produces "two" when (_ == 1)
//    }
//    useCase("another use case") {
      3 produces "three" when (_ == 3)
      4 produces "four"
      5 produces "five"
    }
  }

  val engines = List(engine)
}



object ExampleJUnit {
  def main(args: Array[String]) {
    println("Starting")
    val result = JUnitCore.runClasses(classOf[ExampleJUnit]);
    println("Finished")
    import scala.collection.JavaConversions._
    for (failure <- result.getFailures())
      println(failure);
    println("Sucessful: " + result.wasSuccessful());
    println("RunCount: " + result.getRunCount);
    println("FailureCount: " + result.getFailureCount);

  }
}