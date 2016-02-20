package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{CannotAddScenarioException, Scenario}
import org.cddcore.utilities.CddSpec

class ConflictingScenariosSpec extends CddSpec {

  import Scenario._

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    intercept[CannotAddScenarioException](DecisionTree(Seq(scenario1, scenario2))).getMessage shouldBe
      "Scenario defined at (ConflictingScenariosSpec.scala:12) conflicts with (ConflictingScenariosSpec.scala:11)\n" +
        "Scenario being added is Scenario(situation2 produces result2 JustBecause)/(ConflictingScenariosSpec.scala:12)\n" +
        "Scenario already existing is Scenario(situation1 produces result1 JustBecause)/(ConflictingScenariosSpec.scala:11)\n" + "" +
        "If it was added, would come to result result1"
  }

  "A decistion three with different simple reasons coming to the same conclusions, two of them with a by" should "report an error" in {
    if (FutureWorkFlags.noticingScenariosWithBy) fail
  }


}
