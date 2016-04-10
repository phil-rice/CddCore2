/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.junit.runner.{JUnitCore, RunWith}


@RunWith(classOf[CddRunner])
class ExampleJUnit extends CddContinuousIntegrationTest {
  val engines = List(ExampleJUnit.engine)
}


object ExampleJUnit {

  val engine = new Engine[Int, String]("An engine") {
    useCase("a use case") {
      1 produces "one"
      2 produces "two" when (_ == 1)
    }
    useCase("another use case") {
      3 produces "three" when (_ == 3)
      4 produces "four"
      5 produces "five"
    }
  }

//  def main(args: Array[String]) {
//    println("Starting")
//    val result = JUnitCore.runClasses(classOf[ExampleJUnit])
//    println("Finished")
//    import scala.collection.JavaConversions._
//    for (failure <- result.getFailures())
//      println(failure)
//    println("Sucessful: " + result.wasSuccessful())
//    println("RunCount: " + result.getRunCount)
//    println("FailureCount: " + result.getFailureCount)
//
//  }
}
