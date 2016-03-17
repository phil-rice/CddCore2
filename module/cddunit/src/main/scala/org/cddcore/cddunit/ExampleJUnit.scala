package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.junit.runner.{JUnitCore, RunWith}


@RunWith(classOf[CddRunner])
class ExampleJUnit extends CddContinuousIntegrationTest {

  val engine = new Engine[Int, String] {
    1 produces "one"
    useCase("a use case") {
      2 produces "two"
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