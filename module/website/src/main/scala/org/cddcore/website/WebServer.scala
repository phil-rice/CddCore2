package org.cddcore.website

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.cddcore.engine.Engine
import org.cddcore.rendering.{RenderConfiguration, RenderContext, Renderer}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request, Server}

case class Param(name: String, valueAsString: String, value: Any)

class CddHandler(title: String, engines: List[Engine[_, _]], pathHandlers: List[CddPathHandler], val prefix: String = "")(implicit renderConfiguration: RenderConfiguration) extends AbstractHandler {
  val renderContext: RenderContext = Renderer.renderContext(engines: _*)

   def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
    val uri = baseRequest.getHttpURI
    val fullPath = uri.getPath
    if (fullPath.startsWith(prefix)) {
      val path = fullPath.substring(prefix.length())
      pathHandlers.find(_.willHandle(path)) match {
        case Some(ph) =>
          try {
            baseRequest.setHandled(true);
            response.setContentType("text/html;charset=utf-8");
            response.setStatus(HttpServletResponse.SC_OK);
            val context = new HandlerContext(renderContext, engines, title, baseRequest.getMethod(), path, ph.findUriPath(path))
            val paramsINeed = ph.paramsINeed(context)
            val paramsNameAndValue = paramsINeed.map((name) => (name, baseRequest.getParameter(name)))
            val html = ph.html(context, paramsNameAndValue)
            response.getWriter().println(html)
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

object WebServer {
  def defaultPort = {
    val portString = System.getenv("PORT")
    println("PortString[" + portString + "]")
    val port = portString match { case null => 8080; case _ => portString.toInt }
    println("Port[" + port + "]")
    port
  }

  def defaultPathHandlers = List(new FavIconHandler, new RootPathHandler, new PathHandler)

  def apply(engines: List[Engine[_,_]], title: String = "Engines and Documents", port: Int = defaultPort, handlers: List[CddPathHandler] = defaultPathHandlers)(implicit renderConfiguration: RenderConfiguration) =
    new WebServer(port, new CddHandler(title, engines, handlers)(renderConfiguration))

//  def withPreHandlers(port: Int, engines: List[Engine], preHandlers: CddPathHandler*)(implicit ldp: CddDisplayProcessor): WebServer =
//    new WebServer(port, new CddHandler("Engines and Documents", engines, preHandlers.toList ::: defaultPathHandlers)(ldp))
//
//  def defaultCddHandler(engines: List[Engine])(implicit ldp: CddDisplayProcessor) = new CddHandler("Engines and Documents", engines, defaultPathHandlers)(ldp)

}

class WebServer(val port: Int, val handler: Handler) {

  val server = new Server(port);
  server.setHandler(handler)
  def launch {
    start; server.join
  }
  def start = server.start
  def stop = server.stop
}

