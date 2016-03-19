package org.cddcore.engine

import org.cddcore.enginecomponents.{RememberingLifeCycle, Scenario}

class ConflictingScenariosSpec extends CddNonRecursiveSpec[String, String] {

  import Scenario._

  implicit val lifeCycle = new RememberingLifeCycle[String, String]

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    lifeCycle.errors shouldBe List()
    DecisionTree(mockEngine, Seq(scenario1, scenario2))
    lifeCycle.errors shouldBe List("CannotAddScenarioException/Scenario defined at (ConflictingScenariosSpec.scala:13) conflicts with (ConflictingScenariosSpec.scala:12)\n" +
      "Scenario being added is (ConflictingScenariosSpec.scala:13) situation2 produces result2)\n" +
      "Scenario already existing is (ConflictingScenariosSpec.scala:12) situation1 produces result1)\n" +
      "If it was added, would come to result result1")
  }

  "A decistion tree with different simple reasons coming to the same conclusions, two of them with a by" should "report an error" in {
    if (FutureWorkFlags.noticingScenariosWithBy) fail
  }


}
