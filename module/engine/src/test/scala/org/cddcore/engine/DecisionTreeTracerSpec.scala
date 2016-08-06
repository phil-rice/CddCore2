package org.cddcore.engine

class DecisionTreeTracerSpec extends CddEngineSpec {

  "The DecisionTreeTracer" should "return a list of decision trees, each with one more scenario added" in {
    val e = new Engine[Int, String] {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2)
      3 produces "three" when (_ == 3)
    }
    val List(s1, s2, s3) = e.allScenarios
    s1.situation shouldBe 1

    val List (dt1, dt2, dt3) = DecisionTreeBeingBuiltTracer(e)
    dt1.allScenarios shouldBe List(s1)
  }
}
