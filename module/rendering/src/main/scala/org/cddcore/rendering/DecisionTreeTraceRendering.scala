package org.cddcore.rendering

import java.io.File

import org.cddcore.engine.{DecisionTreeBeingBuiltTracer, Engine}
import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.utilities.Files

object DecisionTreeTraceRendering {

  import Renderer._

  def map[P, R](e: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = {
    val engines = DecisionTreeBeingBuiltTracer(e)
    implicit val renderContext = Renderer.renderContext(engines: _*)
    val engineAndDecisionTreeMaps = engines.foldLeft(List[Map[String, Any]]()) { (acc, engine) =>
      println(s"Engine ${engine.title} has ${engine.allScenarios.size} scenarios")
      val lastScenario = engine.allScenarios.toList.last
      val pathToScenario = engine.withChildrenPaths.find(_.head == lastScenario).get
      val engineMap = Templates.renderPath(renderContext, pathToScenario)
      val decisionTreeMap = DecisionTreeRendering.renderEngine(engine, lastScenario)
      acc :+ engineMap ++ Map(decisionTreeKey -> decisionTreeMap)
    }
    val base = Map("urlBase" -> renderContext.urlBase,
      "iconLinkUrl" -> renderContext.iconLinkUrl,
      "refBase" -> renderContext.referenceFilesUrlBase,
      "decisionTreeBuildTrace" -> engineAndDecisionTreeMaps)
    base + ("json" -> JsonForRendering.pretty(base))
  }

  def html[P, R](e: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) =
    Mustache.apply("templates/DecisionTreeBuilderTrace.mustache").apply(map(e))

  def makeFile[P, R](file: File, e: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = {
    val s = html(e)
    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)
    file.getParentFile.mkdirs()
    println(s"Making it to $file")
    Files.printToFile(file)(pw => pw.println(s))

  }

  def main(args: Array[String]): Unit = {
    val e = new Engine[Int, String] {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2)
      3 produces "three" when (_ == 3)
    }
    implicit val renderConfiguration = RenderConfiguration.defaultRenderConfiguration.copy(referenceFilesUrlBase = "reference")
    makeFile(new File("./target/cdd/dump.html"), e)
  }

}
