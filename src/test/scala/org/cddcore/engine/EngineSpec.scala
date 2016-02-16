package org.cddcore.engine

class EngineSpec extends CddSpec {

  case class Person(wealth: Int)


  "Adding" should "" in {
    val e = new Engine[Person, String] {
      useCase("some usecase") (
        Person(1000) produces "accept" why (_.wealth >= 1000),
        Person(100) produces "reject"
      )
    }
    val List(useCases) = e.builder.useCases
    useCases.scenarios should have size 2

    val List(rich, poor) = e.builder.allScenarios
    poor.situation shouldBe Person(100)
    rich.situation shouldBe Person(1000)
  }
}