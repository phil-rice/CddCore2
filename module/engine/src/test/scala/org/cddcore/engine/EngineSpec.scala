package org.cddcore.engine

import org.cddcore.enginecomponents.UseCase

import scala.collection.immutable.ListMap


class EngineSpec extends CddEngineSpec {

  case class Person(wealth: Int)

  type UC = UseCase[Person, String]
  "The Engine's title" should "be defined by the constructor" in {
    new Engine("someTitle").title shouldBe "someTitle"
    new Engine().title shouldBe "Untitled"
  }

  it should "be settable" in {
    new Engine("someTitle") {
      title("someOtherTitle")
    }.title shouldBe "someOtherTitle"
  }

  "An engine " should "have use cases added " in {
    val e = new Engine[Person, String] {
      useCase("some usecase1")()
      useCase("some usecase2", "comment")()
      useCase("usecase3")()
    }
    val List(uc3, uc2, uc1) = e.asUseCase.components
    uc1 shouldBe UseCase[Person, String]("some usecase1", List(), None, "(EngineSpec.scala:26)", ListMap())
    uc2 shouldBe UseCase[Person, String]("some usecase2", List(), Some("comment"), "(EngineSpec.scala:27)", ListMap())
    uc3 shouldBe UseCase[Person, String]("usecase3", List(), None, "(EngineSpec.scala:28)", ListMap())
  }

  it should "be possible to nest use cases" in {
    val e = safeMake(new Engine[Person, String] {
      useCase("usecase1") {
        useCase("usecase1a", "comment")()
      }
      useCase("usecase2") {
        useCase("usecase2a")()
        useCase("usecase2b")()
      }
    })
    val List(uc2: UC, uc1: UC) = e.asUseCase.components
    val (List(uc1a: UC)) = uc1.components
    val (List(uc2b: UC, uc2a: UC)) = uc2.components
    uc1.title shouldBe "usecase1"
    uc2.title shouldBe "usecase2"
    uc1a.title shouldBe "usecase1a"
    uc2a.title shouldBe "usecase2a"
    uc2b.title shouldBe "usecase2b"
  }
  val e = new Engine[Person, String] {
    useCase("rich people") {
      Person(1000) produces "accept" when (_.wealth >= 1000)
      Person(2000) produces "accept"
    }
    useCase("poor people") {
      Person(100) produces "reject"
      Person(200) produces "reject"
    }
  }

  "Adding scenarios to a usecase" should "appear in the use case" in {
    val List(poorUsecase: UC, richUseCase: UC) = e.asUseCase.components
    richUseCase.title shouldBe "rich people"
    val List(rich1000, rich2000) = richUseCase.allScenarios
    val List(poor100, poor200) = poorUsecase.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)

  }
  "Adding scenarios to a usecase" should "appear in allScenarios" in {
    val List(rich1000, rich2000, poor100, poor200) = e.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)
  }

}