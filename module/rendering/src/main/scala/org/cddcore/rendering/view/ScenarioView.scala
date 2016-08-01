package org.cddcore.rendering.view

import org.cddcore.enginecomponents.Scenario
import org.cddcore.rendering.{ReferenceMapMakers, RenderContext}


class ScenarioView(linkView: LinkView, exceptionView: ExceptionView) extends View[Scenario[_, _]] with ReferenceMapMakers {

  import View._

  override def apply(s: Scenario[_, _])(implicit renderContext: RenderContext): Map[String, Object] = {
    val raw = Map(
      Id -> renderContext.idPath(s),
      Type -> findTypeName(s),
      linkKey -> linkView(s),
      Title -> s.title,
      summaryKey -> renderContext.displayProcessor.summary(s),
      Comment -> s.comment.getOrElse(""),
      situationKey -> renderContext.displayProcessor.html(s.situation),
      expectedKey -> s.expectedOption.map(expected => renderContext.displayProcessor.html(expected)).getOrElse("<Not Known>"),
      References -> s.references.map(referenceToMap(renderContext)))
    renderContext.exceptions.get(s).fold(raw)(e => raw + (Error -> exceptionView(e)))
  }

}
