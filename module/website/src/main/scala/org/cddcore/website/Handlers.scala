package org.cddcore.website

import java.io.{File, InputStream, OutputStream}
import java.net.{URLDecoder, URLEncoder}
import java.nio.file.Files
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.rendering.{Mustache, RenderConfiguration, RenderContext, Renderer}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

import scala.io.Source

class HandlerContext(val renderContext: RenderContext, val engines: List[Engine[_, _]], val title: String, val method: String, val fullUri: String, val uriPath: String)

trait PathHandler {
  def willHandle(uri: String): Boolean

  def findUriPath(uri: String): String = uri

  def paramsINeed(context: HandlerContext): List[String] = List()

  def html(context: HandlerContext, params: List[(String, String)]): String

  def getParam(params: List[(String, String)], name: String) = params.find(_._1 == name).getOrElse(throw new IllegalArgumentException(name))._2
}

class RootPathHandler extends PathHandler {
  def willHandle(uri: String): Boolean = uri == "/"

  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    val json = Map("engines" -> engines.map(engine => Map("title" -> engine.title, "href" -> renderContext.pathMap(engine))),
      "documents" -> engines.flatMap(_.allDocuments).distinct.sortBy(_.ref).map(d => Map("name" -> d.name, "ref" -> d.ref)))
    val html = Mustache("templates/documents.mustache")(json)
    //    val report = Report.documentAndEngineReport(None, reportDate, engines)
    //    val html = Report.html(report, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext)
    html
  }
}


class FavIconHandler extends PathHandler {
  def willHandle(uri: String): Boolean = uri == "/favicon.ico"

  def html(context: HandlerContext, params: List[(String, String)]): String = ""
}

class EngineComponentPathHandler extends PathHandler {
  def willHandle(uri: String): Boolean = true


  def findPath[P, R](uriPath: String)(implicit renderContext: RenderContext): List[EngineComponent[P, R]] = {
    val pathId = URLDecoder.decode(uriPath.drop(1), "UTF-8")
    val Array(engineName, id) = pathId.split("/")
    val idParts = id.split("\\.")
    val raw = for (i <- 1 to idParts.size) yield (engineName + "/" + idParts.take(i).mkString("."))
    val idPaths = raw.reverse :+ (engineName + "/index")

    idPaths.map(renderContext.pathMap.inversePathMap.apply(_).asInstanceOf[EngineComponent[P, R]]).toList
  }

  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    val path = findPath(uriPath)(renderContext)
    //    val engine = path.head
    //    val ss = engine :: engine.allScenarios.toList
    //    println("Scenarios")
    //    ss.map(renderContext.pathMap(_)).foreach(println(_))
    Renderer.makeHtmlFor(path)(renderContext)
  }
}

object ResourcesHandler {

  def copy(source: InputStream, sink: OutputStream) {
    var nread: Long = 0L
    val buf: Array[Byte] = new Array[Byte](1024)
    def read() {
      val n = source.read(buf)
      if (n > 0) {
        sink.write(buf, 0, n)
        nread += n
        read()
      }
    }
    read
    nread
  }
}

class ResourcesHandler(prefix: String) {

  def process(uriPath: String, response: HttpServletResponse) {
    val resourcePath = uriPath.drop(1) // loose the /
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    try {
      ResourcesHandler.copy(stream, response.getOutputStream)
      response.setStatus(HttpServletResponse.SC_OK);
    } finally {
      stream.close()
    }
  }

  def willHandle(uri: String): Boolean = uri.startsWith(prefix)
}

class AggregateHandler(title: String, engines: List[Engine[_, _]], resourceHandlers: List[ResourcesHandler], pathHandlers: List[PathHandler], val prefix: String = "")(implicit renderConfiguration: RenderConfiguration) extends AbstractHandler {
  val renderContext: RenderContext = Renderer.renderContext(engines: _*)
  println(s"Starting aggregate handler. UriBase is: ${renderConfiguration.urlBase} // ${renderContext.urlBase}")

  def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
    val uri = baseRequest.getHttpURI
    val fullPath = uri.getPath
    if (fullPath.startsWith(prefix)) {
      val path = fullPath.substring(prefix.length())
      resourceHandlers.find(_.willHandle(path)) match {
        case Some(rh) => {
          rh.process(path, response);
          baseRequest.setHandled(true)
        }
        case None =>
          pathHandlers.find(_.willHandle(path)) match {
            case Some(ph) =>
              try {
                baseRequest.setHandled(true)

                val context = new HandlerContext(renderContext, engines, title, baseRequest.getMethod(), path, ph.findUriPath(path))
                val paramsINeed = ph.paramsINeed(context)
                val paramsNameAndValue = paramsINeed.map((name) => (name, baseRequest.getParameter(name)))
                val html = ph.html(context, paramsNameAndValue)
                response.getWriter().println(html)
                response.setContentType("text/html;charset=utf-8")
                response.setStatus(HttpServletResponse.SC_OK)
              } catch {
                case e: Throwable =>
                  println(ph)
                  e.printStackTrace()
                  throw e
              }
            case _ => ;
          }
      }
    }
  }
}
