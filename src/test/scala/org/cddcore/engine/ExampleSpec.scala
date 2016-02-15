package org.cddcore.engine

import org.scalatest.{FlatSpec, Matchers}

class ExampleSpec extends CddSpec {


  case class Person(wealth: Int)


  "The richPoor example" should "accept rich people and reject poor people" in {
    //    val richPoor = new Engine[Person, String]("Rich Poor") {
    //      useCase("Rich People are accepted") {
    //        scenario(Person(1000)) { case p if p.wealth > 1000 => "accepted" }
    //        scenario(Person(2000))("accepted")
    //      }
    //      useCase("Poor People are rejected") {
    //        scenario(Person(100))("rejected")
    //      }
    //    }
//    val richPoor = new Engine[Person, String]("Rich Poor") {
//      useCase("Rich People are accepted") {
//        new ScenarioBuilder[Person, String](Person(1000) produces "accepted") because { case p: Person if p.wealth > 1000 => "accepted" }
//        scenario(Person(2000))("accepted")
//      }
//      useCase("Poor People are rejected") {
//        scenario(Person(100))("rejected")
//      }
//    }

//    richPoor(Person(1000)) shouldBe "accepted"
//    richPoor(Person(2000)) shouldBe "accepted"
//    richPoor(Person(100)) shouldBe "rejected"
  }
}


