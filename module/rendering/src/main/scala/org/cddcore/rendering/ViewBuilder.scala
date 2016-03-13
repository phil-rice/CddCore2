package org.cddcore.rendering

import java.io.StringWriter

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

  implicit def ecToViewBuilder(ec: EC)(implicit renderConfiguration: RenderConfiguration) = new EngineComponentPimper(ec)

  def pathMap(ec: EC) = PathMap(ec)

  def renderContext(ec: EC)(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) =
    RenderContext(renderConfiguration.date, renderConfiguration.urlBase, pathMap(ec))

  def withDescendents(ec: EC): List[EC] = ec :: Templates.findChildren(ec).flatMap(withDescendents)

  def scalaMap(ec: EC)(implicit rc: RenderContext): Map[String, Any] = Templates.renderData(rc, ec)

  def templateName(ec: EC) = s"${Templates.findTypeName(ec)}.mustache"

  def javaMap(ec: EC)(implicit rc: RenderContext) = Templates.forMustache(scalaMap(ec))

  def toHtml(ec: EC)(implicit rc: RenderContext = renderContext(ec)) = {
    Mustache.apply(templateName(ec)).apply(ec)
  }

  def mapList[X](list: List[Any], fn: EC => X): List[Any] = list.map { x =>
    x match {
      case l: List[_] => mapList(l, fn)
      case ec: EC => fn(ec)
      case x => x
    }
  }

  def main(args: Array[String]) {
    implicit val rc = engineWithUseCase.renderContext
    val paths = engineWithUseCase.withChildrenPaths.map(path => path.map(_.title).mkString(","))
    println("paths")
    println(paths.mkString("\n"))
    println
    println
    println
    println

    val maps = engineWithUseCase.withChildrenPaths.map(Templates.renderPath(rc, _))
    println(maps.map(JsonForRendering.pretty).zip(paths).map { case (json, path) => path + "\n" + json }.mkString("\n\n"))
    println
    println
    //    println(scenario1.toSingleMaps.map(m => Mustache.apply("Report.mustache").apply(m)).mkString("\n\n"))
    //    println(useCase1.toSingleMaps.map(m => Mustache.apply("Report.mustache").apply(m)).mkString("\n\n"))
    println(maps.zip(paths).map { case (m, path) => path + "\n" + Mustache.apply("Report.mustache").apply(m) }.mkString("\n\n"))


    for (path <- engineWithUseCase.withChildrenPaths) {
      val map = Templates.renderPath(rc, path)
      val html = Mustache.apply("Report.mustache").apply(map)
      rc.makeFile(path.head, html)
    }
  }
}

class EngineComponentPimper(ec: EngineComponent[_, _]) {
  type EC = EngineComponent[_, _]

  def renderContext(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) = Renderer.renderContext((ec))

  def toHtml(implicit rc: RenderContext = renderContext) = Renderer.toHtml(ec)


  def toJavaMap(implicit renderContext: RenderContext = renderContext) = Renderer.javaMap(ec)

  def toMap(implicit renderContext: RenderContext = renderContext) = Renderer.scalaMap(ec)

  def withChildrenPaths(implicit renderContext: RenderContext = renderContext): List[List[EC]] = withChildrenPaths(ec, List())

  protected def withChildrenPaths(ec: EngineComponent[_, _], path: List[EC])(implicit renderContext: RenderContext): List[List[EC]] = {
    val thisPath = ec :: path
    thisPath :: Templates.findChildren(ec).flatMap(withChildrenPaths(_, thisPath))
  }

  def withDescendents = Renderer.withDescendents(ec)

  def withChildrenPathMaps(implicit renderContext: RenderContext = renderContext) =
    withChildrenPaths.map(path => path.map { case ec: EC => Renderer.scalaMap(ec) })

  def toSingleMaps(implicit renderContext: RenderContext = renderContext) =
    withChildrenPathMaps.map(path => Map(
      "id" -> path.head("id"),
      path.head("type") -> path.reduce((acc, v) => v + ("path" -> acc)))

    )

}



