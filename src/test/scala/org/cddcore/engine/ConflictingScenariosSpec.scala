package org.cddcore.engine

class ConflictingScenariosSpec extends CddSpec {

  import Scenario._

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    intercept[CannotAddScenarioException](DecisionTree(Seq(scenario1, scenario2))).getMessage shouldBe
      "Scenario defined at (ConflictingScenariosSpec.scala:9) conflicts with (ConflictingScenariosSpec.scala:8)\n" +
        "Scenario being added is Scenario(situation2 produces result2)\n" +
        "Scenario already existing is Scenario(situation1 produces result1)"
  }

}