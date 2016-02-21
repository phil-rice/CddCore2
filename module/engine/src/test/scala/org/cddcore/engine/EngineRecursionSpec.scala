package org.cddcore.engine

class EngineRecursionSpec extends CddEngineSpec {

  "An engine" should "have mocks for all situations that have results" in {
    val e = new Engine[Int, Int] {
      1 produces 1
      2 produces last * 2
      3 produces last * 3
      4 produces last * 4
    }
    e.builder.mocks shouldBe Map(1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24)
  }

  it should "not have mocks if only an assertion is available" in {
    val e = new Engine[Int, Int] {
      1 produces 1
      2 produces last * 2
      5 produces something where (_ => true)
      3 produces 6
      7 produces something where (_ => true)
      4 produces 24
    }
    e.builder.mocks shouldBe Map(1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24)
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


  it should "explain it hasn't got a mock value if needed, when only one scenario" in {
    intercept[RecursiveScenariosWithoutMocksException](
      new Engine[Int, Int] {
        2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
      }.decisionTree).getMessage should startWith(expectedMsgPrefix(74))

  }
  it should "explain it hasn't got a mock value if needed, when only multiple scenarios, first one not matched by situation" in {
    intercept[RecursiveScenariosWithoutMocksException](new Engine[Int, Int] {
      -1 produces 0
      2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
    }.decisionTree).getMessage should startWith(expectedMsgPrefix(81))
  }

  it should "explain it hasn't got a mock value if needed, when only multiple scenarios, first one matched by situation" in {
    intercept[RecursiveScenariosWithoutMocksException](
      new Engine[Int, Int] {
        7 produces 0
        2 produces 2 byRecursion { case (engine, i) if i > 1 => i * engine(i - 1) }
      }.decisionTree).getMessage should startWith(expectedMsgPrefix(89))
  }

}
