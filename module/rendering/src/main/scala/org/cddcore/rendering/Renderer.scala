/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import org.cddcore.engine.{Engine, Trace}
import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.utilities.{DisplayProcessor, Strings}

import scala.collection.immutable.ListMap

object Renderer extends ExpectedForTemplates {
  type EC = EngineComponent[_, _]

  implicit def engineToPimper[P, R](ec: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = new EnginePimper(ec)

  implicit def engineComponentToPimper[P, R](ec: EngineComponent[P, R])(implicit renderConfiguration: RenderConfiguration) = new EngineComponentPimper(ec)

  def pathMap(engines: Engine[_, _]*) = PathMap(engines)

  def renderContext(engines: Engine[_, _]*)(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) = {
    val exceptions = engines.foldLeft(Map[EngineComponent[_, _], Exception]()) { (acc, engine) => acc ++ engine.errors.toMap }
    RenderContext(renderConfiguration.date,
      renderConfiguration.urlBase, renderConfiguration.referenceFilesUrlBase, renderConfiguration.iconLinkUrl,
      pathMap(engines: _*),exceptions, renderConfiguration.urlManipulations)
  }

  def withDescendents(ec: EC): List[EC] = ec :: Templates.findChildren(ec).flatMap(withDescendents)

  def scalaMap(ec: EC)(implicit rc: RenderContext): Map[String, Any] = Templates.renderData(rc, ec)

  def templateName(ec: EC) = s"${Templates.findTypeName(ec)}.mustache"

  def toHtml(ec: Engine[_, _])(implicit rc: RenderContext = renderContext(ec)) = {
    Mustache.apply(templateName(ec)).apply(ec)
  }

  def mapList[X](list: List[Any], fn: EC => X): List[Any] = list.map { x =>
    x match {
      case l: List[_] => mapList(l, fn)
      case ec: EC => fn(ec)
      case x => x
    }
  }

  def makeReportFilesFor[P, R](iconLinkUrl: String, urlOffset: String, referenceBase: String, engine: Engine[P, R])(implicit renderConfiguration: RenderConfiguration): RenderContext = {
    import Strings._
    val newRenderConfiguration = renderConfiguration.copy(
      iconLinkUrl = iconLinkUrl,
      urlBase = uri(renderConfiguration.urlBase, urlOffset),
      referenceFilesUrlBase = referenceBase)
    makeReportFilesFor(engine)(newRenderConfiguration)
  }

  def makeHtmlFor[P, R](path: List[EngineComponent[P, R]])(implicit renderContext: RenderContext) = {
    val engine = path.last.asInstanceOf[Engine[P, R]]
    val engineMap = Templates.renderPath(renderContext, path)
    val decisionTreeMap = DecisionTreeRendering.renderEngine(engine, path.head)
    val withJson = engineMap ++ Map(
      "urlBase" -> renderContext.urlBase,
      "iconLinkUrl" -> renderContext.iconLinkUrl,
      "refBase" -> renderContext.referenceFilesUrlBase,
      decisionTreeKey -> decisionTreeMap,
      "json" -> (JsonForRendering.pretty(decisionTreeMap) + "\n\n\n\n\n" + JsonForRendering.pretty(engineMap)))
    Mustache.apply("templates/Report.mustache").apply(withJson)

  }

  def makeReportFilesFor[P, R](engine: Engine[P, R])(implicit renderConfiguration: RenderConfiguration): RenderContext = {
    implicit val rc = renderContext(engine)
    //    rc.urlManipulations.populateInitialFiles(rc.referenceFilesUrlBase)
    for (path <- engine.withChildrenPaths) {
      val html = makeHtmlFor(path)
      //      val engineMap = Templates.renderPath(rc, path)
      //      val decisionTreeMap = DecisionTreeRendering.renderEngine(engine, path.head)
      //      val withJson = engineMap ++ Map(
      //        decisionTreeKey -> decisionTreeMap,
      //        "json" -> (JsonForRendering.pretty(decisionTreeMap) + "\n\n\n\n\n" + JsonForRendering.pretty(engineMap)))
      //      val html = Mustache.apply("templates/Report.mustache").apply(withJson)
      rc.makeFile(path.head, html)
    }
    rc
  }

  def makeHtmlFor(trace: Trace)(implicit renderContext: RenderContext): String = {
    val json = TraceRendering.renderTrace(renderContext, parentTrace)
    Mustache("templates/TraceReport.mustache")(Map(
      traceKey -> json,
      "urlBase" -> renderContext.urlBase,
      "json" -> JsonForRendering.pretty(json)))

  }

  def main(args: Array[String]) {
    makeReportFilesFor(engineWithUseCase)
    makeReportFilesFor(engineNested)
    implicit val rc = renderContext(engineNested, engineWithUseCase)
    val json = TraceRendering.renderTrace(rc, parentTrace)
    val html = makeHtmlFor(parentTrace)(rc)

    rc.urlManipulations.populateInitialFiles(rc.referenceFilesUrlBase)
    rc.urlManipulations.makeFile(rc.urlBase + "/trace/test.html", html)
  }
}
