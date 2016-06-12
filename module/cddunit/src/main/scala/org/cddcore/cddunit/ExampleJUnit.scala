/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.junit.runner.{JUnitCore, RunWith}


@RunWith(classOf[CddRunner])
class ExampleJUnit extends CddContinuousIntegrationTest {
  val engines = List(ExampleJUnit.engine1)
}

@RunWith(classOf[CddRunner])
class ExampleJUnit2 extends CddContinuousIntegrationTest {
  val engines = List(ExampleJUnit.invalidScenarios, ExampleJUnit.redundantMain)
}

@RunWith(classOf[CddRunner])
class ExampleJUnit3 extends CddContinuousIntegrationTest {
  val engines = List(ExampleJUnit.engine3)
}


object ExampleJUnit {

  val engine1 = new Engine[Int, String]("An engine") {
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

  val invalidScenarios = new Engine[String, String]("Invalid") {
    "ref" produces "result"
    useCase("Reason invalid") {
      "a21" produces "result" when (_ => false)
      "a22" produces "result" because { case "a" => "result" }
    }
    useCase("Produced value invalid invalid") {
      "a23" produces "result" by (_ => "different")
      "a24" produces "result" because { case "a24" => "different" }
    }
  }

  val malformedScenarios = new Engine[String, String]("Malformed") {
    "ref" produces "result"
    "ref1" produces "result" when (_ == "ref1") by (_ => "result") because { case "ref1" => "result" } withComment "when by and because"
    "ref2" produces "result" by (_ => "result") by (_ => "result") withComment ("second by")
    "ref3" produces "result" because { case "ref3" => "result" } because { case "ref3" => "result" } withComment ("second because")
    "ref4" produces "result" by (_ => "result")byRecursion { case (engine, "ref") => "result" } byRecursion { case (engine, "ref") => "result" } withComment("second byRecursion")
    "ref5" produces "result" when (_ == "ref1")when (_ == "ref1") withComment("second when")
  }

  val redundantNeither = new Engine[String, String]("Redundant-neither have allow merge") {
    useCase("Reference") {
      "ab11" produces "result" when (_ contains "a")
    }
    useCase("Redundant reason") {
      "ab31" produces "result" when (_ contains ("a"))
      "ab32" produces "result" because { case x if x contains ("a") => "result" }
    }
  }
  val redundantMain = new Engine[String, String]("Redundant-main has allow merge") {
    useCase("Reference") {
      "ab11" produces "result" when (_ contains "a") allows merge
    }
    useCase("Redundant reason") {
      "ab31" produces "result" when (_ contains ("a"))
      "ab32" produces "result" because { case x if x contains ("a") => "result" }
    }
  }
  val redundantS = new Engine[String, String]("Redundant-new has allow merge") {
    useCase("Reference") {
      "ab11" produces "result" when (_ contains "a")
    }
    useCase("Redundant reason") {
      "ab31" produces "result" when (_ contains ("a")) allows merge
      "ab32" produces "result" because { case x if x contains ("a") => "result" } allows merge
    }
  }

  val conflictingNoReasons = new Engine[String, String]("Conflicting - No Reasons") {
    useCase("Reference") {
      "ref" produces "result"
    }
    useCase("Conflict") {
      "ref" produces "different"
    }
  }
  val conflictingMainHasReasons = new Engine[String, String]("Conflicting - Main has reason") {
    useCase("Reference") {
      "ref" produces "result" when (_ == "ref")
    }
    useCase("Conflict") {
      "ref" produces "different"
    }
  }
  val conflictingNewHasReasons = new Engine[String, String]("Conflicting - New has reason") {
    useCase("Reference") {
      "ref" produces "result"
    }
    useCase("Conflict ") {
      "ref" produces "different" when (_ == "ref")
    }
  }
  val conflictingBothReasons = new Engine[String, String]("Conflicting - Both have reasons") {
    useCase("Reference") {
      "ref" produces "result" when (_ == "ref")
    }
    useCase("Conflict") {
      "ref" produces "different" when (_ == "ref")
    }
  }

  val engine3 = new Engine[Int, String]("Engine 3") {
    useCase("use case 4") {
      1 produces "one"
      2 produces "two" when (_ == 1)
    }
    useCase("use case 5") {
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
