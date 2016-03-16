package org.cddcore.rendering

import org.cddcore.utilities.CddSpec

class PathMapSpec extends CddSpec with TestObjectsForRendering {

  "PathMap" should "map from engine components to a <number>.<number>.... path" in {
    val pathMap = PathMap(List(engineWithUseCase))
    pathMap(engineWithUseCase) shouldBe "someEngineTitle2/index"
    pathMap(useCase1) shouldBe "someEngineTitle2/1"
    pathMap(scenario1) shouldBe "someEngineTitle2/1.1"
    pathMap(scenario2) shouldBe "someEngineTitle2/1.2"
    pathMap(scenario3) shouldBe "someEngineTitle2/2.1"
    pathMap(scenario4) shouldBe "someEngineTitle2/3"
  }
}
