package org.cddcore.rendering

import java.util.Date

import org.cddcore.enginecomponents.EngineComponent
import org.cddcore.utilities.DisplayProcessor

case class RenderConfiguration(date: Date = new Date,
                               urlBase: String = "./target/cdd",
                               referenceFilesUrlBase: String = "./target/cdd/reference",
                               iconLinkUrl: String = "./target/cdd/index.html",
                               urlManipulations: UrlManipulations = new FileUrlManipulations)

object RenderConfiguration {
  implicit val defaultRenderConfiguration = RenderConfiguration()
}


case class RenderContext(reportDate: Date,
                         urlBase: String, referenceFilesUrlBase: String, iconLinkUrl: String,
                         pathMap: PathMap,
                         exceptions: Map[EngineComponent[_,_], Exception],
                         urlManipulations: UrlManipulations)(implicit val displayProcessor: DisplayProcessor) {
  override def toString = getClass.getSimpleName()

  val inversePathMap = pathMap.inversePathMap

  def idPath(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlManipulations.makeUrl(urlBase, idPath(ec))

  def makeFile(ec: EngineComponent[_, _], textForEngine: String) = urlManipulations.makeFile(url(ec), textForEngine)
}
