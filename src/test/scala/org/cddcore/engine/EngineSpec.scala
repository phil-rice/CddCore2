package org.cddcore.engine

class EngineSpec extends CddSpec {

  case class Person(wealth: Int)

  "The Engine's title" should "be defined by the constructor" in {
    new Engine("someTitle").builder.title shouldBe "someTitle"
    new Engine().builder.title shouldBe "Untitled"
  }

  it should "be settable" in {
    new Engine("someTitle") {
      title("someOtherTitle")
    }.builder.title shouldBe "someOtherTitle"
  }

  "An engine " should "have use cases added " in {
    val e = new Engine[Person, String] {
      useCase("some usecase1")()
      useCase("some usecase2", "comment")()
      useCase("usecase3")()
    }
    val List(uc1, uc2, uc3) = e.builder.useCases
    uc1 shouldBe UseCase("some usecase1", List(), None, "(EngineSpec.scala:20)")
    uc2 shouldBe UseCase("some usecase2", List(), Some("comment"), "(EngineSpec.scala:21)")
    uc3 shouldBe UseCase("usecase3", List(), None, "(EngineSpec.scala:22)")

  }
  val e = new Engine[Person, String] {
    useCase("rich people")(
      Person(1000) produces "accept" when (_.wealth >= 1000),
      Person(2000) produces "accept"
    )
    useCase("poor people")(
      Person(100) produces "reject",
      Person(200) produces "reject"
    )
  }

  "Adding scenarios to a usecase" should "appear in the use case" in {
    val List(richUseCase, poorUsecase) = e.builder.useCases
    val List(rich1000, rich2000) = richUseCase.allScenarios
    val List(poor100, poor200) = poorUsecase.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)

  }
  "Adding scenarios to a usecase" should "appear in allScenarios" in {
    val List(rich1000, rich2000, poor100, poor200) = e.builder.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)
  }

}