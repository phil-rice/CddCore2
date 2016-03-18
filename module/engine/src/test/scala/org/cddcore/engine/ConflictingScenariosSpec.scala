package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{RememberingLifeCycle, EngineComponent, CannotAddScenarioException, Scenario}
import org.cddcore.utilities.NullLifeCycle

class ConflictingScenariosSpec extends CddNonRecursiveSpec[String, String] {

  import Scenario._

  implicit val lifeCycle = new RememberingLifeCycle[String, String]

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    lifeCycle.errors shouldBe List()
    DecisionTree(mockEngine, Seq(scenario1, scenario2))
    lifeCycle.errors shouldBe List("CannotAddScenarioException/Scenario defined at (ConflictingScenariosSpec.scala:14) conflicts with (ConflictingScenariosSpec.scala:13)\n" +
      "Scenario being added is Scenario (ConflictingScenariosSpec.scala:14) situation2 produces result2)\n" +
      "Scenario already existing is Scenario (ConflictingScenariosSpec.scala:13) situation1 produces result1)\n" +
      "If it was added, would come to result result1")
  }

  "A decistion tree with different simple reasons coming to the same conclusions, two of them with a by" should "report an error" in {
    if (FutureWorkFlags.noticingScenariosWithBy) fail
  }


}
