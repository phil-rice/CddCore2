package org.cddcore.rendering.view

import org.cddcore.engine.{CddEngineSpec, InvalidScenariosTestFramework}
import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.rendering.Renderer


class LinkViewSpec extends InvalidScenariosTestFramework {

  import View._

  def withLinkView(fn: (LinkView) => Unit) = {
    fn(new LinkView(new IconUrlFinder))
  }

  "A LinkView" should "make a map that has the title, link url, iconurl and defined at from the engine component it is given" in {
    withLinkView { (linkView) =>
      implicit val rc = Renderer.renderContext(invalidScenarios).copy(referenceFilesUrlBase = "base")

      val map = linkView(invalidScenarios)
      map(Title) shouldBe "Invalid"
      map(LinkUrl) shouldBe rc.url(invalidScenarios)
      map(IconUrl) shouldBe "base/images/engine.png"
      map(DefinedAt) shouldBe invalidScenarios.definedInSourceCodeAt.toString
    }
  }
}
