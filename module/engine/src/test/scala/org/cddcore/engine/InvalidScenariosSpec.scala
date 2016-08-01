package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}


trait InvalidScenariosTestFramework extends CddEngineSpec {
  val invalidScenarios = new Engine[String, String]("Invalid") {
    useCase("Reason invalid") {
      "reason1" produces "result" when (_ => false)
      "reason2" produces "result" because { case "wrong" => "result" }
    }
    useCase("Produced value invalid ") {
      "produces1" produces "result" by (_ => "different")
      "produces2" produces "result" because { case _ => "different" }
    }
    useCase("nesting to make sure errors are propogated") {
      useCase("nesting even deeper") {
        useCase("when other things after error") {
          "reasonProduces1" produces "result" when (_ => false) by (_ => "different")
          "reasonProduces2" produces "result" when (_ => false) withComment ("someComment2")
          "reasonProduces3" produces "result" when (_ => false) by (_ => "different") withComment ("someComment3")
          "reasonProduces4" produces "result" when (_ => false) by (_ => "different") withComment ("someComment3") allows merge
        }
      }
    }
    useCase("duplicating clauses") {
      "ref1" produces "result" when (_ == "ref1") by (_ => "result") because { case "ref1" => "result" } withComment "when by and because"
      "ref2" produces "result" by (_ => "result") by (_ => "result") withComment ("second by")
      "ref3" produces "result" because { case "ref3" => "result" } because { case "ref3" => "result" } withComment ("second because")
      "ref4" produces "result" by (_ => "result") byRecursion { case (engine, "ref") => "result" } byRecursion { case (engine, "ref") => "result" } withComment ("second byRecursion")
      "ref5" produces "result" when (_ == "ref5") when (_ == "ref5") withComment ("second when")
    }
  }

  val conflictingNoReasons = new Engine[String, String]("Conflicting - No Reasons") {
    useCase("Reference") {
      "conflicting1" produces "result"
    }
    useCase("Conflict") {
      "conflicting2" produces "different"
    }
  }
  val conflictingMainHasReasons = new Engine[String, String]("Conflicting - Main has reason") {
    useCase("Reference") {
      "mainHasReason1" produces "result" when (_ contains "Reason")
    }
    useCase("Conflict") {
      "mainHasReason2" produces "different"
    }
  }
  val conflictingNewHasReasons = new Engine[String, String]("Conflicting - New has reason") {
    useCase("Reference") {
      "newHasReason1" produces "result"
    }
    useCase("Conflict ") {
      "newHasReason2" produces "different" when (_ contains "Reason")
    }
  }
  val conflictingBothReasons = new Engine[String, String]("Conflicting - Both have reasons") {
    useCase("Reference") {
      "bothHaveReasons1" produces "result" when (_ contains "Reason")
    }
    useCase("Conflict") {
      "bothHaveReasons2" produces "different" when (_ contains "Reason")
    }
  }


   val allEngines: List[Engine[String, String]] = List(invalidScenarios, conflictingNoReasons, conflictingMainHasReasons, conflictingNewHasReasons, conflictingBothReasons)
  val allScenarios = allEngines.flatMap(_.allScenarios)

  val allErrors = {
    allEngines.foreach(_.decisionTree)
    allEngines.map(_.errors).foldLeft(Map[EngineComponent[_, _], Exception]())((acc, errors) => acc ++ errors)
  }

  lazy val definedErrorsList = s"Errors are\n${allErrors.collect { case (k: Scenario[_, _], v) => k.situation + "/" + k.definedInSourceCodeAt -> v.getClass.getSimpleName }.mkString("\n")}"

  def withErrorClue[X](block: => X) = withClue(definedErrorsList)(block)

  def checkScenarioException(situation: String, exceptionName: String) =
    withClue(s"Scenario:$situation") {
      val s = scenario(situation)
      withErrorClue(allErrors.get(s).map(_.getClass.getSimpleName).get shouldBe exceptionName)
      s
    }

  def scenario(situation: String) = allScenarios.find(_.situation == situation).getOrElse(fail(s"Legal values are ${allScenarios.map(_.situation)}"))

  def exception[X](situation: String) = withErrorClue[X](allErrors(scenario(situation)).asInstanceOf[X])
}

class InvalidScenariosSpec extends CddEngineSpec with InvalidScenariosTestFramework {
  "Invalid scenarios" should "still be added" in {
    checkScenarioException("reason1", "ReasonInvalidException")
    checkScenarioException("reason2", "ReasonInvalidException")
    checkScenarioException("produces1", "AssertionInvalidException")
    checkScenarioException("produces2", "AssertionInvalidException")
    checkScenarioException("reasonProduces1", "ReasonInvalidException")
    checkScenarioException("reasonProduces2", "ReasonInvalidException").comment shouldBe Some("someComment2")
    checkScenarioException("reasonProduces3", "ReasonInvalidException").comment shouldBe Some("someComment3")
    checkScenarioException("reasonProduces4", "ReasonInvalidException").canMerge shouldBe true
    checkScenarioException("ref1", "ScenarioCannotHaveWhenByAndBecauseException")
    checkScenarioException("ref2", "ScenarioCannotHaveSecondByException")
    checkScenarioException("ref3", "ScenarioCannotHaveSecondBecauseException")
    checkScenarioException("ref4", "ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException")
    checkScenarioException("ref5", "ScenarioCannotHaveSecondWhenException")
    checkScenarioException("ref5", "ScenarioCannotHaveSecondWhenException")
    checkScenarioException("ref5", "ScenarioCannotHaveSecondWhenException")
  }
}
