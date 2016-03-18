package org.cddcore.engine.enginecomponents

import java.io.File

import org.cddcore.utilities.{DisplayProcessor, Hierarchy, ToSummary}

class AddedFinderNotActuallAnException extends Exception

object EngineComponent {
  protected val defaultStackTraceOffset = 2

  def definedInSourceCodeAt(stackTraceOffset: Int = defaultStackTraceOffset) = {
    val st = new AddedFinderNotActuallAnException().getStackTrace()(stackTraceOffset).toString
    val i = st.lastIndexOf("(")
    st.substring(i)
  }

}

trait EngineComponent[P, R] {
  def definedInSourceCodeAt: String

  def allScenarios: TraversableOnce[Scenario[P, R]]

  def title: String
}

trait HasComment {
  def comment: Option[String]
}

object UseCase {
  implicit def useCaseHierarcy[P, R] =
    new Hierarchy[UseCase[P, R], EngineComponent[P, R]] {
      def withNewChild(h: UseCase[P, R], child: EngineComponent[P, R]): UseCase[P, R] =
        h.copy(components = child :: h.components)

      def modChild(h: UseCase[P, R], fn: (EngineComponent[P, R]) => EngineComponent[P, R]) = h.components match {
        case oldHead :: tail => h.copy(components = fn(oldHead) :: tail)
      }

      def lastAddedChild(h: UseCase[P, R]) = h.components.headOption

      def childToHolder(child: EngineComponent[P, R]): UseCase[P, R] = child.asInstanceOf[UseCase[P, R]]

      def badChild(topParent: UseCase[P, R], child: EngineComponent[P, R], exception: Exception): UseCase[P, R] = ???
    }
}

case class UseCase[P, R](title: String, components: List[EngineComponent[P, R]] = List(), comment: Option[String] = None, definedInSourceCodeAt: String) extends EngineComponent[P, R] with ToSummary with HasComment {
  def allScenarios = components.reverse.flatMap(_.allScenarios)

  override def toSummary(displayProcessor: DisplayProcessor): String = s"UseCase($title${comment.map(c => s",$c").getOrElse("")})"
}
