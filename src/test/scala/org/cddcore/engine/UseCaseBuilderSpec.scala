package org.cddcore.engine

class UseCaseBuilderSpec extends CddSpec {
  def uc(s: String, ec: EngineComponent[Int, String]*) = UseCase[Int, String](s, ec.toList, None, "")

  val useCase1 = uc("useCase1")
  val useCase2 = uc("useCase2")
  val useCase3 = uc("useCase3")
  val useCase4 = uc("useCase4")

  import Scenario._

  val s1 = 1 produces "result"
  val s2 = 2 produces "result"
  val s3 = 3 produces "result"


  "A UseCaseBuilder with no operations" should "have the passed in use case and depth 0" in {
    val holder1 = new UseCaseBuilder[Int, String](useCase1)
    holder1.useCase shouldBe useCase1
    holder1.depth shouldBe 0
  }

  "A UseCaseBuilder addChild method with depth 0" should "add children to the use case and not mess with depth" in {
    val holder1 = new UseCaseBuilder[Int, String](useCase1)
    val holder2 = holder1.addChild(s1).addChild(s2).addChild(s3)
    holder2.useCase shouldBe useCase1.copy(components = List(s3, s2, s1))
    holder2.depth shouldBe 0
  }
  "A UseCaseBuilder addNewParent method " should "nest children with new usecases increasing depth" in {
    val holder1 = new UseCaseBuilder[Int, String](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3)
    holder2.useCase shouldBe uc("useCase1", uc("useCase2", uc("useCase3")))
    holder2.depth shouldBe 2
  }
  it should "allow scenarios to be added to current use case" in {
    val holder1 = new UseCaseBuilder[Int, String](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3).addChild(s1).addChild(s2).addChild(s3)
    holder2.useCase shouldBe uc("useCase1", uc("useCase2", uc("useCase3", s3, s2, s1)))
    holder2.depth shouldBe 2
  }
  it should "allow scenarios to be added to current use case, then a pop and another use case added" in {
    val holder1 = new UseCaseBuilder[Int, String](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3).addChild(s1).popParent
    holder2.depth shouldBe 1
    val holder3 = holder2.addNewParent(useCase4).addChild(s2).addChild(s3)
    holder3.useCase shouldBe uc("useCase1", uc("useCase2", uc("useCase4", s3, s2), uc("useCase3", s1)))
    holder3.depth shouldBe 2
  }


}
