/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.ChildLifeCycle

class EngineRecursionSpec extends CddEngineSpec {

  class EngineWithRawMocksExposed[P, R] extends Engine[P, R] {
    def actualRawMocks = rawMocks
  }

  "An engine" should "have mocks for all situations that have results" in {
    val e = new EngineWithRawMocksExposed[Int, Int] {
      1 produces 1
      2 produces last * 2
      3 produces last * 3
      4 produces last * 4
    }
    e.actualRawMocks shouldBe Map(1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24)
  }

  it should "not have mocks if only an assertion is available" in {
    val e = new EngineWithRawMocksExposed[Int, Int] {
      1 produces 1
      2 produces last * 2
      5 produces something where (_ => true)
      3 produces 6
      7 produces something where (_ => true)
      4 produces 24
    }
    e.actualRawMocks shouldBe Map(1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24)
  }

  "A recursive engine" should "compute the expected values" in {
    val e = new Engine[Int, Int] {
      1 produces 1
      2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
      3 produces 6
    }
    e.validate

    e(1) shouldBe 1
    e(2) shouldBe 2
    e(3) shouldBe 6
    e(4) shouldBe 24
    e(10) shouldBe 3628800
  }

  it should "allow multiple recursion reasons" in {
    val e = new Engine[Int, Int] {
      1 produces 1
      2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
      3 produces 6
      4 produces 10 byRecursion { case (engine, i) if i > 3 => i + engine(i - 1) }
      5 produces 15
    }
    e.validate

    e(1) shouldBe 1
    e(2) shouldBe 2
    e(3) shouldBe 6
    e(4) shouldBe 10
    e(5) shouldBe 15
    e(6) shouldBe 21

  }

  def expectedMsgPrefix(lineNo: Int) =
    "The following scenarios don't have a mock:\n" +
      s"Mock for situation[1] needed by Scenario(2 produces 2 byRecursion {case (_1: Int => Int, _2: Int)(Int => Int, Int)((engine @ _), (i @ _)) if i.>(1) => i.*(engine.apply(i.-(1)))})/(EngineRecursionSpec.scala:$lineNo)\n" +
      "Valid mocks are:"

  //  "The following scenarios don't have a mock:\n" +
  //    s"Scenario(2 produces 2 byRecursion {case (_1: Int => Int, _2: Int)(Int => Int, Int)((engine @ _), (i @ _)) if i.>(1) => i.*(engine.apply(i.-(1)))})/(EngineRecursionSpec.scala:$lineNo)\n" +
  //    "Valid mocks are:\n"


  def checkForRecursiveScenariosWithoutMocks[P, R](block: ChildLifeCycle[EngineComponent[P, R]] => Scenario[P, R]) = {
    val (e, lastScenario, errors) = toErrors[P, R](block)
    errors shouldBe Map()
    e.decisionTree
    mapErrorsToClassName(e.asUseCase.errors) shouldBe Map(lastScenario -> "MockValueNotFoundException")
  }

  import Scenario._

  it should "explain it hasn't got a mock value if needed, when only one scenario" in {
    //    intercept[RecursiveScenariosWithoutMocksException](
    //      new Engine[Int, Int] {
    //        2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
    //      }.decisionTree).getMessage should include(expectedMsgPrefix(88))


    checkForRecursiveScenariosWithoutMocks[Int, Int](implicit clc => 2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) })
  }
  it should "explain it hasn't got a mock value if needed, when only multiple scenarios, first one not matched by situation" in {
    checkForRecursiveScenariosWithoutMocks[Int, Int] { implicit clc =>
      -1 produces 0
      2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
    }
  }

  it should "explain it hasn't got a mock value if needed, when only multiple scenarios, first one matched by situation" in {
    checkForRecursiveScenariosWithoutMocks[Int, Int] { implicit clc =>
      7 produces 0
      2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
    }

  }

}
