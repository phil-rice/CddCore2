package org.cddcore.rendering.view

import org.cddcore.engine.{CddEngineSpec, InvalidScenariosTestFramework}
import org.cddcore.rendering.Renderer


class IconUrlFinderSpec extends InvalidScenariosTestFramework {
  val finder = new IconUrlFinder();

  implicit val renderContext = Renderer.renderContext(allEngines: _*).copy(referenceFilesUrlBase = "base")

  "The IconUrlFinder" should "return an engine icon for an engine" in {
    finder.findIconUrl(invalidScenarios) shouldBe "base/images/engine.png"
  }

  it should "return a use case icon for a usecase" in {
    finder.findIconUrl(invalidScenarios.asUseCase) shouldBe "base/images/usecase.png"
  }

  it should "return a scenario icon for a scenario that doesn't have an exception" in {
    finder.findIconUrl(scenario("conflicting1")) shouldBe "base/images/scenario.png"
  }

  it should "return a error scenario icon for a scenario that has ane exception" in {
    finder.findIconUrl(scenario("conflicting2")) shouldBe "base/images/errorScenario.png"
  }
}
