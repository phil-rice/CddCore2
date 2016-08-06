package org.cddcore.rendering

import org.cddcore.engine.{CddEngineSpec, Engine}

import scala.xml.XML


class DecisionTreeTraceRendererSpec extends CddEngineSpec {
  "DecisionTreeTraceRenderer" should "produce a single report with the Engine/scenario on the left, DT at the right, one per scenario being added" in {
    val e = new Engine[Int, String] {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2)
      3 produces "three" when (_ == 3)
    }
    val List(s1, s2, s3) = e.allScenarios.toList
    implicit val renderContext = Renderer.renderContext(e)
    val html = XML.loadString(DecisionTreeTraceRendering.html(e))
  }
}
