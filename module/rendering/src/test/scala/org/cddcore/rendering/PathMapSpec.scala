package org.cddcore.rendering

import org.cddcore.utilities.CddSpec

class PathMapSpec extends CddSpec with TestObjectsForRendering {

  "PathMap" should "map from engine components to a <number>.<number>.... path" in {
    val pathMap = PathMap(List(engineWithUseCase))
    pathMap(engineWithUseCase) shouldBe "1"
    pathMap(useCase1) shouldBe "1.1"
    pathMap(scenario1) shouldBe "1.1.1"
    pathMap(scenario2) shouldBe "1.1.2"
    pathMap(scenario3) shouldBe "1.1.3"
  }
}
