package org.cddcore.rendering

import mustache.Mustache
import org.cddcore.utilities.CddSpec


class MustacheTemplateSpec extends CddSpec with TestObjectsForRendering with Icons with ExpectedForTemplates with MustacheTemplates{

  "A link template" should "display a link" in {
    linkTemplate.render(expectedForScenario1Link) shouldBe s"<a href='urlBase/Scenario/1.1.1' alt='1'>1<img src='$scenarioIcon' /></a>"
  }

}
