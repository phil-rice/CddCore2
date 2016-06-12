package org.cddcore.engine


class InvalidScenariosSpec extends CddEngineSpec {

  val invalidScenarios = new Engine[String, String]("Invalid") {
    useCase("Reason invalid") {
      "reason1" produces "result" when (_ => false)
      "reason2" produces "result" because { case "wrong" => "result" }
    }
    useCase("Produced value invalid ") {
      "produces1" produces "result" by (_ => "different")
      "produces2" produces "result" because { case _ => "different" }
    }

    useCase ("when other things after error") {
      "reasonProduces1" produces "result" when (_ => false) by (_ => "different")
      "reasonProduces2" produces "result" when (_ => false) withComment("someComment2")
      "reasonProduces3" produces "result" when (_ => false) by (_ => "different") withComment("someComment3")
      "reasonProduces3" produces "result" when (_ => false) by (_ => "different") withComment("someComment3")  allows merge
    }
    useCase("duplicating clauses") {
      "ref1" produces "result" when (_ == "ref1") by (_ => "result") because { case "ref1" => "result" } withComment "when by and because"
      "ref2" produces "result" by (_ => "result") by (_ => "result") withComment ("second by")
      "ref3" produces "result" because { case "ref3" => "result" } because { case "ref3" => "result" } withComment ("second because")
      "ref4" produces "result" by (_ => "result") byRecursion { case (engine, "ref") => "result" } byRecursion { case (engine, "ref") => "result" } withComment ("second byRecursion")
      "ref5" produces "result" when (_ == "ref1") when (_ == "ref1") withComment ("second when")
    }
  }

  val allScenarios = invalidScenarios.allScenarios.toList

  def scenario(situation: String, exceptionName: String) =
    withClue(s"Scenario:$situation") {
      val s = allScenarios.find(_.situation == situation).getOrElse(fail(s"Legal values are ${allScenarios.map(_.situation)}"))
      invalidScenarios.errors(s).getClass.getSimpleName shouldBe exceptionName
      s
    }

  "Invalid scenarios" should "still be added" in {
    scenario("reason1", "ReasonInvalidException")
    scenario("reason2", "ReasonInvalidException")
    scenario("produces1", "AssertionInvalidException")
    scenario("produces2", "AssertionInvalidException")
    scenario("reasonProduces1", "ReasonInvalidException")
    scenario("reasonProduces2", "ReasonInvalidException").comment shouldBe "someComment2"
    scenario("reasonProduces3", "ReasonInvalidException").comment shouldBe "someComment3"
    scenario("reasonProduces3", "ReasonInvalidException").canMerge shouldBe true
    scenario("ref1", "")
    scenario("ref2", "")
    scenario("ref3", "")
    scenario("ref4", "")
    scenario("ref5", "")
  }
}
