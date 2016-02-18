package org.cddcore.engine

class Engine2Spec extends CddSpec {

  "An Engine with no use cases" should "have no scenarios" in {
    new Engine2[Int, String]().allScenarios.toList shouldBe List()

  }
  it should "have a definedInSourceCodeAt" in {
    new Engine2[Int, String]().definedInSourceCodeAt shouldBe "(Engine2Spec.scala:10)"
  }

  "An Engine" should "allow a single blank use cases to be specified" in {
    val e = new Engine2[Int, String]() {
      useCase("someUseCase") {}
    }
  }

}
