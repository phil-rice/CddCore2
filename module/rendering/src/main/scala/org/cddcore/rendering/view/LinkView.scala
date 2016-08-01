package org.cddcore.rendering.view


import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.rendering.{Icons, RenderContext}
import org.cddcore.utilities.Strings._

class IconUrlFinder {

  import Icons._

  def findIconUrl(e: EngineComponent[_, _])(implicit rc: RenderContext) = e match {
    case e: Engine[_, _] => uri(rc.referenceFilesUrlBase, engineWithTestsIcon)
    case uc: UseCase[_, _] => uri(rc.referenceFilesUrlBase, useCasesIcon)
    case s: Scenario[_, _] if rc.exceptions.contains(s) => uri(rc.referenceFilesUrlBase, errorScenarioIcon)
    case s: Scenario[_, _] => uri(rc.referenceFilesUrlBase, scenarioIcon)
  }
}

class LinkView(iconUrlFinder: IconUrlFinder) extends View[EngineComponent[_, _]] {
import View._
  override def apply(ec: EngineComponent[_, _])(implicit rc: RenderContext): Map[String, Object] =
    Map(Title -> ec.title,
      LinkUrl -> rc.url(ec),
      IconUrl -> iconUrlFinder.findIconUrl(ec),
      DefinedAt -> ec.definedInSourceCodeAt.toString)

}
