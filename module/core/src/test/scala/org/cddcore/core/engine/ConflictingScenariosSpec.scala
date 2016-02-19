package org.cddcore.core.engine


class ConflictingScenariosSpec extends CddSpec {

  import Scenario._

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    intercept[CannotAddScenarioException](DecisionTree(Seq(scenario1, scenario2))).getMessage shouldBe
      "Scenario defined at (ConflictingScenariosSpec.scala:10) conflicts with (ConflictingScenariosSpec.scala:9)\n" +
        "Scenario being added is Scenario(situation2 produces EqualsAssertion(result2) because SimpleReason(result2))/(ConflictingScenariosSpec.scala:10)\n" +
        "Scenario already existing is Scenario(situation1 produces EqualsAssertion(result1) because SimpleReason(result1))/(ConflictingScenariosSpec.scala:9)\n" +
        "If it was added, would come to result result1"
  }

}
