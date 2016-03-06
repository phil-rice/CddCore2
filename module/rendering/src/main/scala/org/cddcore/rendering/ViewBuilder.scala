package org.cddcore.rendering

import java.io.StringWriter

import org.cddcore.engine.{Engine2, Engine, AbstractEngine}
import org.cddcore.engine.enginecomponents.{DisplayProcessor, Scenario, UseCase, EngineComponent}


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

  def renderContext(ec: EC)(implicit renderConfiguration: RenderConfiguration) =
    RenderContext(renderConfiguration.date, renderConfiguration.urlBase, pathMap(ec))

  def withDescendents(ec: EC): List[EC] = ec :: Templates.findChildren(ec).flatMap(withDescendents)

  def scalaMap(ec: EC)(implicit rc: RenderContext): Map[String, Any] = Templates.renderDepth1(rc, ec)

  def templateName(ec: EC) = s"${Templates.findTypeName(ec)}.mustache"

  def javaMap(ec: EC)(implicit rc: RenderContext) = Templates.forMustache(scalaMap(ec))

  def toHtml(ec: EC)(implicit rc: RenderContext = renderContext(ec)) = {
    val writer = new StringWriter()
    Mustache.apply(templateName(ec)).execute(writer, javaMap(ec))
    writer.flush()
    writer.toString
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
    println(useCase1.toSingleMaps)
  }
}

class EngineComponentPimper(ec: EngineComponent[_, _]) {
  type EC = EngineComponent[_, _]

  def renderContext(implicit renderConfiguration: RenderConfiguration) = Renderer.renderContext((ec))

  def toHtml(implicit rc: RenderContext = renderContext) = Renderer.toHtml(ec)

  def withChildrenPaths(implicit renderContext: RenderContext = renderContext): List[List[EC]] = withChildrenPaths(ec, List())

  def toJavaMap(implicit renderContext: RenderContext = renderContext) = Renderer.javaMap(ec)

  def toMap(implicit renderContext: RenderContext = renderContext) = Renderer.scalaMap(ec)

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



