package org.cddcore.website

import java.net.{URLDecoder, URLEncoder}

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.rendering.{Mustache, RenderContext, Renderer}

class HandlerContext(val renderContext: RenderContext, val engines: List[Engine[_, _]], val title: String, val method: String, val fullUri: String, val uriPath: String) {
  //  val urlMap = renderContext.urlMap
  //  lazy val path: List[Reportable] = urlMap(uriPath)
  //  lazy val engine = findEngine(path)
}

trait CddPathHandler {
  def willHandle(uri: String): Boolean

  def findUriPath(uri: String): String = uri

  def paramsINeed(context: HandlerContext): List[String] = List()

  def html(context: HandlerContext, params: List[(String, String)]): String

  def getParam(params: List[(String, String)], name: String) = params.find(_._1 == name).getOrElse(throw new IllegalArgumentException(name))._2
}

class RootPathHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri == "/"

  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    val json = Map("engines" -> engines.map(engine => Map("title" -> engine.title, "href" -> renderContext.pathMap(engine))))
    val html = Mustache("templates/documents.mustache")(json)
    //    val report = Report.documentAndEngineReport(None, reportDate, engines)
    //    val html = Report.html(report, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext)
    html
  }
}


class FavIconHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri == "/favicon.ico"

  def html(context: HandlerContext, params: List[(String, String)]): String = ""
}

class PathHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = true

  def findPath[P, R](uriPath: String)(implicit renderContext: RenderContext) = {
    val pathId = URLDecoder.decode(uriPath.drop(1), "UTF-8")
    val pathFragments = pathId.split("/").toList
    val engineId = pathFragments(0) + "/index"
    val restOfPath = pathFragments match {
      case (_ :: "index" :: Nil) => Nil
      case _ => pathFragments.tail
    }
    val pathAsIds = engineId :: restOfPath
    println("Path as Ids: " + pathAsIds)

    val path = pathAsIds.map(renderContext.pathMap.inversePathMap.apply(_).asInstanceOf[EngineComponent[P, R]]).reverse
    println("Path: \n" + path.map(e => "  " + e.title + " - " + e.definedInSourceCodeAt + " - " + e.getClass).mkString("\n"))
    path


  }


  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    //    println("Uri Path: " + context.uriPath)
    val path = findPath(uriPath)(renderContext)
    val engine = path.head
    val ss = engine :: engine.allScenarios.toList
    println("Scenarios")
    ss.map(renderContext.pathMap(_)).foreach(println(_))
    Renderer.makeHtmlFor(path)(renderContext)

  }
}
