package org.cddcore.rendering

import java.io.{File, StringWriter}

import org.cddcore.engine.Engine
import org.cddcore.engine.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.utilities.DisplayProcessor


case class TemplateNames(engineName: String, useCaseName: String, scenarioName: String) {
  def nameFor(ec: EngineComponent[_, _]) =
    ec match {
      case _: Engine[_, _] => engineName
      case _: UseCase[_, _] => useCaseName
      case _: Scenario[_, _] => scenarioName
    }
}

object Renderer extends ExpectedForTemplates {
  type EC = EngineComponent[_, _]

  implicit def engineToPimper[P, R](ec: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = new EnginePimper(ec)
  implicit def engineComponentToPimper[P, R](ec: EngineComponent[P, R])(implicit renderConfiguration: RenderConfiguration) = new EngineComponentPimper(ec)

  def pathMap(engines: Engine[_, _]*) = PathMap(engines)

  def renderContext(engines: Engine[_, _]*)(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) =
    RenderContext(renderConfiguration.date, renderConfiguration.urlBase, pathMap(engines: _*), renderConfiguration.urlManipulations)

  def withDescendents(ec: EC): List[EC] = ec :: Templates.findChildren(ec).flatMap(withDescendents)

  def scalaMap(ec: EC)(implicit rc: RenderContext): Map[String, Any] = Templates.renderData(rc, ec)

  def templateName(ec: EC) = s"${Templates.findTypeName(ec)}.mustache"

  //  def javaMap(ec: EC)(implicit rc: RenderContext) = Templates.forMustache(scalaMap(ec))

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

  def makeReportFilesFor[P, R](engine: Engine[P, R])(implicit renderConfiguration: RenderConfiguration) = {
    implicit val rc = renderContext(engine)
    rc.urlManipulations.populateInitialFiles(rc.urlBase)
    for (path <- engine.withChildrenPaths) {
      val engineMap = Templates.renderPath(rc, path)
      val decisionTreeMap = DecisionTreeRendering.renderEngine(engine, path.head)
      val withJson = engineMap ++ Map(
        decisionTreeKey -> decisionTreeMap,
        "json" -> (JsonForRendering.pretty(decisionTreeMap) + "\n\n\n\n\n" + JsonForRendering.pretty(engineMap)))
      val html = Mustache.apply("templates/Report.mustache").apply(withJson)
      rc.makeFile(path.head, html)
    }
  }

  def main(args: Array[String]) {
    makeReportFilesFor(engineWithUseCase)
  }

}

class EnginePimper[P, R](ec: Engine[P, R]) {

  def renderContext(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) = Renderer.renderContext((ec))

  def toHtml(implicit rc: RenderContext = renderContext) = Renderer.toHtml(ec)


  //  def toJavaMap(implicit renderContext: RenderContext = renderContext) = Renderer.javaMap(ec)

  //  def toSingleMaps(implicit renderContext: RenderContext = renderContext) =
  //    withChildrenPathMaps.map(path => Map(
  //      "id" -> path.head("id"),
  //      path.head("type") -> path.reduce((acc, v) => v + ("path" -> acc)))
  //
  //    )
}

class EngineComponentPimper[P, R](ec: EngineComponent[P, R]) {
  type EC = EngineComponent[P, R]

  def toMap(implicit renderContext: RenderContext) = Renderer.scalaMap(ec)

  def withChildrenPaths(implicit renderContext: RenderContext): List[List[EC]] = withChildrenPaths(ec, List())

  protected def withChildrenPaths(ec: EngineComponent[P, R], path: List[EC])(implicit renderContext: RenderContext): List[List[EC]] = {
    val thisPath = ec :: path
    thisPath :: Templates.findChildren(ec).asInstanceOf[List[EC]].flatMap(withChildrenPaths(_, thisPath))
  }

  def withDescendents = Renderer.withDescendents(ec)

  def withChildrenPathMaps(implicit renderContext: RenderContext) = withChildrenPaths.map(path => path.map { case ec: EC => Renderer.scalaMap(ec) })


}


