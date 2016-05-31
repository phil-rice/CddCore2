package org.cddcore.testinterface

import org.cddcore.engine.Engine
import org.cddcore.rendering.{JsonForRendering, Mustache, RenderConfiguration, Renderer}
import org.cddcore.utilities.Strings


case class ClassAndTestDetails(className: String, engineTitleAndUrls: List[(String, String)])

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

  private def makeEngineFilesReturningListOfTitleAndUrl =
    classNameToEngines.foldLeft(List[ClassAndTestDetails]()) { case (acc, (testClassName, engines)) =>
      val engineDetails = engines.foldLeft(List[(String, String)]()) { (acc, engine) =>
        val rc = Renderer.makeReportFilesFor("../../index.html", testClassName, "../../reference", engine)
        acc :+(engine.title, rc.url(engine))
      }
      ClassAndTestDetails(testClassName, engineDetails) :: acc
    }.sortBy(_.className)

  private def makeIndexFile(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val classMap = Map[String, Any](
      "title" -> "Tests",
      "refBase" -> "./reference",
      "iconLink" -> "index.html",
      "urlBase" -> ".",
      "testClass" -> classAndTestDetails.map { case ClassAndTestDetails(c, engines) =>
        Map("title" -> c, "url" -> (c + "/index.html"),
          "engines" -> engines.map(e => Map()))
      })

    val html = Mustache.apply("templates/TestIndex.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
    renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, "index.html"), html)
  }

  private def makeTestClassFiles(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) =
    classNameToEngines.foreach { case (testClassName, engines) =>
      for (ClassAndTestDetails(className, engineTitleAndUrls) <- classAndTestDetails) {
        val classMap = Map[String, Any](
          "title" -> testClassName,
          "refBase" -> "../reference",
          "iconLinkUrl" -> "../index.html",
          "urlBase" -> ".",
          "engines" -> engineTitleAndUrls.map { case (title, url) => Map("title" -> title, "url" -> url) })

        val html = Mustache.apply("templates/TestClass.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
        renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, testClassName, "index.html"), html)
      }
    }

}

