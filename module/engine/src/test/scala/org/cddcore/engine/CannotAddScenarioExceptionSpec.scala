package org.cddcore.engine


class CannotAddScenarioExceptionSpec extends CddEngineSpec {

  "An engine that cannot add a scenario" should "give advice on what to do when the neither have a reason" in {

    val e = new Engine[Int, String] {
      1 produces "one"
      2 produces "two"
    }
    val Seq(s1, s2) = e.allScenarios
    e.buildDecisionTree
    //    e.apply(1) shouldBe "one"
    e.errors.size shouldBe 1
    val cannotAdd = e.errors(s2).asInstanceOf[CannotAddScenarioException[Int, String]]
    cannotAdd.existing shouldBe s1
    cannotAdd.scenario shouldBe s2
    cannotAdd.actual shouldBe "one"
    cannotAdd.getMessage shouldBe
      """Scenario defined at (CannotAddScenarioExceptionSpec.scala:10) conflicts with (CannotAddScenarioExceptionSpec.scala:9)
        |Scenario being added is (CannotAddScenarioExceptionSpec.scala:10) 2 produces two)
        |Scenario already existing is (CannotAddScenarioExceptionSpec.scala:9) 1 produces one)
        |If it was added, would come to result
        |  one""".stripMargin
    cannotAdd.advice shouldBe "Neither of these scenarios has a reason. A reason could be added to either scenario with a 'when' or a 'because'"

  }

}
