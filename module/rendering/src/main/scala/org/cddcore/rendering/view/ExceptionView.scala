package org.cddcore.rendering.view

import org.cddcore.enginecomponents._
import org.cddcore.rendering.RenderContext
import org.cddcore.utilities.Maps


class ExceptionView extends View[Exception] {
  import View._
  override def apply(e: Exception)(implicit renderContext: RenderContext): Map[String, Object] = {
    val raw = Map(Class -> e.getClass.getSimpleName, Stack -> e.getStackTrace.take(5).mkString("\n"))
    val withMessage = e match {
      case se: ScenarioException[_, _] => raw + (Message -> se.mainMessage)
      case _ => raw + (Message -> e.getMessage)
    }
    Maps.addToMap[Exception, String, Object](
      { case ha: HasActual[_] => Actual -> renderContext.displayProcessor.html(ha.actual) }, //
      { case he: HasExplaination => Explaination -> he.explaination.mkString("<br />") }, //
      { case wa: HasAdvice => Advice -> wa.advice.mkString("<br />") }, //
      { case r: HasReason => Reason -> r.reason })(withMessage, e)
  }
}
