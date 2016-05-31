package org.cddcore.rendering

import org.cddcore.engine.Engine
import org.cddcore.utilities.Strings


case class ClassAndTestDetails(className: String, engineAndUrls: List[(Engine[_, _], String)])

class TestStructure {

  import org.cddcore.utilities.MapOfLists._

  private var classNameToEngines = Map[String, List[Engine[_, _]]]()

  def add(className: String, engines: List[Engine[_, _]]) = engines.foreach(engine => classNameToEngines = classNameToEngines.addToList(className, engine))

  def makeFiles(implicit renderConfiguration: RenderConfiguration) = {
    makeReferenceFiles
    val classAndTestDetails = makeEngineFilesReturningListOfTitleAndUrl
    makeTestClassFiles(classAndTestDetails)
    makeIndexFile(classAndTestDetails)
  }

  private def makeReferenceFiles(implicit renderConfiguration: RenderConfiguration) = {
    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)
  }

  private def engineAsMap(tuple: (Engine[_, _], String)) = tuple match {
    case (engine, url) => Map("title" -> engine.title, "url" -> url
    )
  }

  private def makeEngineFilesReturningListOfTitleAndUrl =
    classNameToEngines.foldLeft(List[ClassAndTestDetails]()) { case (acc, (testClassName, engines)) =>
      val engineDetails = engines.foldLeft(List[(Engine[_, _], String)]()) { (acc, engine) =>
        val rc = Renderer.makeReportFilesFor("../../index.html", testClassName, "../../reference", engine)
        acc :+(engine, rc.url(engine))
      }
      ClassAndTestDetails(testClassName, engineDetails) :: acc
    }.sortBy(_.className)

  private def makeIndexFile(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val classMap = Map[String, Any](
      "title" -> "Tests",
      "refBase" -> "./reference",
      "iconLink" -> "index.html",
      "urlBase" -> ".",
      "testClass" -> classAndTestDetails.map { case ClassAndTestDetails(c, engineAndUrls) =>
        Map("title" -> c, "url" -> (c + "/index.html"), "engines" -> engineAndUrls.map(engineAsMap))
      })

    val html = Mustache.apply("templates/TestIndex.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
    renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, "index.html"), html)
  }

  private def makeTestClassFiles(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) =
    classNameToEngines.foreach { case (testClassName, engines) =>
      for (ClassAndTestDetails(className, engineAndUrls) <- classAndTestDetails) {
        val classMap = Map[String, Any](
          "title" -> testClassName,
          "refBase" -> "../reference",
          "iconLinkUrl" -> "../index.html",
          "urlBase" -> ".",
          "engines" -> engineAndUrls.map(engineAsMap))

        val html = Mustache.apply("templates/TestClass.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
        renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, testClassName, "index.html"), html)
      }
    }

}

