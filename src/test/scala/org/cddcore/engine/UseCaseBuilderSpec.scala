package org.cddcore.engine

class UseCaseBuilderSpec extends CddSpec {
  val useCase1 = UseCase[Int, String]("useCase1", List(), None, "addedBytTest")
  val useCase1a = UseCase[Int, String]("useCase1a", List(), None, "addedBytTest")
  val useCase1b = UseCase[Int, String]("useCase1a", List(), None, "addedBytTest")
  val useCase1With1A = useCase1.copy(components = List(useCase1a))
  val useCase1With1AAnd1B = useCase1.copy(components = List(useCase1a, useCase1b))

  val useCase2 = UseCase[Int, String]("useCase2", List(), None, "addedBytTest")

  val blank = new UseCaseBuilder[Int, String](List())
  val withOne = blank.addUseCase(useCase1)

  "A use case builder that has no use cases added" should "build and empty list" in {
    blank.useCases shouldBe List()
  }

  it should "not have a currentUseCase" in {
    blank.currentUseCase shouldBe None
  }

  it should "get a currentUseCase and that should be in '.useCases' when one is added" in {
    withOne.useCases shouldBe List(useCase1)
    withOne.currentUseCase shouldBe Some(useCase1)
  }

  it should "throw when addChildUseCase is called" in {
    intercept[RuntimeException](blank.addChild(useCase1))
  }

  "A use case holder with one use case" should "add a child use case when one is added, and stay the current one" in {
    val withOneAndOnea = withOne.addChild(useCase1a)

    withOneAndOnea.currentUseCase shouldBe Some(useCase1With1A)
    withOneAndOnea.useCases shouldBe List(useCase1With1A)
  }


}
