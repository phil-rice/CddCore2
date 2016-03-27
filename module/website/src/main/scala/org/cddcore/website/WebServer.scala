package org.cddcore.website

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import org.cddcore.engine.Engine
import org.cddcore.rendering.{RenderConfiguration, RenderContext, Renderer, WebsiteUrlManipulators}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Handler, Request, Server}

case class Param(name: String, valueAsString: String, value: Any)

object WebServer {
  def defaultPort = {
    val portString = System.getenv("PORT")
    println("PortString[" + portString + "]")
    val port = portString match {
      case null => 8080;
      case _ => portString.toInt
    }
    println("Port[" + port + "]")
    port
  }

  def resourceHandlers = List(new ResourcesHandler("/images"), new ResourcesHandler("/stylesheets"))

  def defaultPathHandlers = List(new FavIconHandler, new RootPathHandler, new EngineComponentPathHandler)

  def apply(engines: List[Engine[_, _]], title: String = "Engines and Documents", port: Int = defaultPort, handlers: List[PathHandler] = defaultPathHandlers)(implicit renderConfiguration: RenderConfiguration) =
    new WebServer(port, new AggregateHandler(title, engines, resourceHandlers, handlers)(renderConfiguration.copy(urlBase = "/", urlManipulations = new WebsiteUrlManipulators)))

  //  def withPreHandlers(port: Int, engines: List[Engine], preHandlers: CddPathHandler*)(implicit ldp: CddDisplayProcessor): WebServer =
  //    new WebServer(port, new CddHandler("Engines and Documents", engines, preHandlers.toList ::: defaultPathHandlers)(ldp))
  //
  //  def defaultCddHandler(engines: List[Engine])(implicit ldp: CddDisplayProcessor) = new CddHandler("Engines and Documents", engines, defaultPathHandlers)(ldp)

}

class WebServer(val port: Int, val handler: Handler) {

  val server = new Server(port);
  server.setHandler(handler)

  def launch {
    start;
    server.join
  }

  def start = server.start

  def stop = server.stop
}

