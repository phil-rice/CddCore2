package org.cddcore.rendering

import org.cddcore.utilities.CddSpec


class TestObjectsForRenderingSpec extends CddSpec with TestObjectsForRendering{

  "The usecases" should "be the ones in order" in {
    useCase1.title shouldBe "someUseCase"
  }

  "The scenarios" should "be the ones in order" in {
    scenario1.situation shouldBe 1
    scenario2.situation shouldBe 2
    scenario3.situation shouldBe 3
  }

}
