/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.Strings

trait TestView {
  def engineAsMap(tuple: (Engine[_, _], RenderContext)) = tuple match {
    case (engine, rc) => Map(
      "title" -> engine.title,
      "url" -> rc.url(engine),
      "engineFailed" -> (engine.errors.size > 0),
      "errorCount" -> engine.errors.size,
      "errors" -> engine.errors.map {
        case (ec: Scenario[_, _], error) => Map(
          "definedAt" -> ec.definedInSourceCodeAt.toString,
          "description" -> (rc.displayProcessor.summary(ec) + " / " + error.getClass.getSimpleName),
          "url" -> rc.url(ec))
      })
  }
}

case class ClassAndTestDetails(className: String, engineAndRCss: List[(Engine[_, _], RenderContext)])

class TestIndexView extends TestView {
  def makeIndexMap(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val engineMaps = classAndTestDetails.map {
      case ClassAndTestDetails(c, engineAndUrls) => Map("title" -> c, "url" -> (c + "/index.html"), "engines" -> engineAndUrls.map(engineAsMap))
    }
    Map[String, Any](
      "title" -> "Tests",
      "refBase" -> "./reference",
      "iconLink" -> "index.html",
      "urlBase" -> ".",
      "testClass" -> engineMaps)
  }

  def makeIndexHtml(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val classMap = makeIndexMap(classAndTestDetails)
    Mustache.apply("templates/TestIndex.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
  }
}

class TestClassView extends TestView {
  def makeTestClassMaps(classAndTestDetails: ClassAndTestDetails)(implicit renderConfiguration: RenderConfiguration): Map[String, Any] = classAndTestDetails match {
    case ClassAndTestDetails(className, engineAndRCs) =>
      val classMap = Map[String, Any](
        "title" -> className,
        "refBase" -> "../reference",
        "iconLinkUrl" -> "../index.html",
        "urlBase" -> ".",
        "engines" -> engineAndRCs.map(engineAsMap))
      classMap + ("json" -> JsonForRendering.pretty(classMap))
  }


  def makeTestClassHtml(classAndTestDetails: ClassAndTestDetails)(implicit renderConfiguration: RenderConfiguration) =
    Mustache.apply("templates/TestClass.mustache")(makeTestClassMaps(classAndTestDetails))

}


class TestViews(testIndexView: TestIndexView, testClassView: TestClassView) {
  def makeIndexFile(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) = {
    val html = testIndexView.makeIndexHtml(classAndTestDetails)
    renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, "index.html"), html)
  }

  def makeTestClassFiles(classAndTestDetails: List[ClassAndTestDetails])(implicit renderConfiguration: RenderConfiguration) =
    for (details@ClassAndTestDetails(className, engineAndRCs) <- classAndTestDetails) {
      val html = testClassView.makeTestClassHtml(details)
      renderConfiguration.urlManipulations.makeFile(Strings.uri(renderConfiguration.urlBase, className, "index.html"), html)
    }

  def engineAsMap(tuple: (Engine[_, _], RenderContext)) = tuple match {
    case (engine, rc) => Map(
      "title" -> engine.title,
      "url" -> rc.url(engine),
      "engineFailed" -> (engine.errors.size > 0),
      "errorCount" -> engine.errors.size,
      "errors" -> engine.errors.map {
        case (ec: Scenario[_, _], error) => Map(
          "definedAt" -> ec.definedInSourceCodeAt.toString,
          "description" -> rc.displayProcessor.summary(ec),
          "url" -> rc.url(ec))
      })
  }
}

class TestStructure(testViews: TestViews) {

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
    testViews.makeTestClassFiles(classAndTestDetails)
    testViews.makeIndexFile(classAndTestDetails)
  } catch {
    case e: Exception => e.printStackTrace(); throw e
  }

  private def makeReferenceFiles(implicit renderConfiguration: RenderConfiguration) = {
    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)
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


}

