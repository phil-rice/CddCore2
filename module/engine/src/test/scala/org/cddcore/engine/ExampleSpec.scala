package org.cddcore.engine

class ExampleSpec extends CddEngineSpec {

  case class Person(wealth: Int)


  "The richPoor example" should "accept rich people and reject poor people" in {
    val richPoor = new Engine[Person, String]("Rich Poor") {
      useCase("Poor People are rejected") {
        Person(100) produces "rejected"
      }
      useCase("Rich People are accepted") {
        Person(1000) produces "accept" when (_.wealth >= 1000)
        Person(2000) produces "accept"
      }
    }
    richPoor(Person(1000)) shouldBe "accept"
    richPoor(Person(2000)) shouldBe "accept"
    richPoor(Person(100)) shouldBe "rejected"

  }
}


