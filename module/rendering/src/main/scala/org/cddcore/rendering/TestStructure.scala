package org.cddcore.rendering

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.Strings


case class ClassAndTestDetails(className: String, engineAndRCss: List[(Engine[_, _], RenderContext)])

class TestStructure {

  import org.cddcore.utilities.MapOfLists._

  private var classNameToEngines = Map[String, List[Engine[_, _]]]()

  def add(className: String, engines: List[Engine[_, _]]) = this.synchronized {
    engines.foreach { engine =>
      classNameToEngines = classNameToEngines.addToList(className, engine)
    }
  }

  def makeFiles(implicit renderConfiguration: RenderConfiguration) = try {

    makeReferenceFiles
    val classAndTestDetails = makeEngineFilesReturningListOfTitleAndUrl
    makeTestClassFiles(classAndTestDetails)
    makeIndexFile(classAndTestDetails)
  } catch {
    case e: Exception => e.printStackTrace(); throw e
  }

  private def makeReferenceFiles(implicit renderConfiguration: RenderConfiguration) = {
    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)
  }

  private def engineAsMap(tuple: (Engine[_, _], RenderContext)) = tuple match {
    case (engine, rc) => Map(
      "title" -> engine.title,
      "url" -> rc.url(engine),
      "engineFailed" -> (engine.errors.size > 0),
      "errorCount" -> engine.errors.size,
      "errors" -> engine.errors.map {
        case (ec: Scenario[_, _], error) => Map(
          "definedAt" -> ec.definedInSourceCodeAt.toString,
          "description" -> ec.toString,
          "url" -> rc.url(ec))
      })
  }

  private def makeEngineFilesReturningListOfTitleAndUrl: List[ClassAndTestDetails] = {
    val enginesAndRcs = for {(testClassName, engines) <- classNameToEngines
                             engine <- engines
                             rc = Renderer.makeReportFilesFor("../../index.html", testClassName, "../../reference", engine)
    } yield {
      (engine, rc)
    }

    classNameToEngines.map { case (testClassName, engines) =>
      val engineDetails = engines.map { engine =>
        val rc = Renderer.makeReportFilesFor("../../index.html", testClassName, "../../reference", engine)
        (engine, rc)
      }
      ClassAndTestDetails(testClassName, engineDetails)
    }.toList.sortBy(_.className)
  }

  private def makeIndexFile(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val engineMaps = classAndTestDetails.map {
      case ClassAndTestDetails(c, engineAndUrls) => Map("title" -> c, "url" -> (c + "/index.html"), "engines" -> engineAndUrls.map(engineAsMap))
    }
    if (engineMaps.size != classAndTestDetails.size) {
      System.exit(1)
    }
    val classMap = Map[String, Any](
      "title" -> "Tests",
      "refBase" -> "./reference",
      "iconLink" -> "index.html",
      "urlBase" -> ".",
      "testClass" -> engineMaps)

    val html = Mustache.apply("templates/TestIndex.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
    renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, "index.html"), html)
  }

  private def makeTestClassFiles(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) =
    for (ClassAndTestDetails(className, engineAndRCs) <- classAndTestDetails) {
      val classMap = Map[String, Any](
        "title" -> className,
        "refBase" -> "../reference",
        "iconLinkUrl" -> "../index.html",
        "urlBase" -> ".",
        "engines" -> engineAndRCs.map(engineAsMap))

      val html = Mustache.apply("templates/TestClass.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
      renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, className, "index.html"), html)
    }

}

