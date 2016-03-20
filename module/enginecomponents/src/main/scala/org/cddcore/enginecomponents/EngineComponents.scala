package org.cddcore.enginecomponents

import org.cddcore.utilities.{DisplayProcessor, Hierarchy, ToSummary}

import scala.collection.immutable.ListMap

class AddedFinderNotActuallAnException extends Exception

object EngineComponent {


}

object DefinedInSourceCodeAt {
  protected val defaultStackTraceOffset = 2

  def definedInSourceCodeAt(stackTraceOffset: Int = defaultStackTraceOffset) =
    new DefinedInSourceCodeAt(new AddedFinderNotActuallAnException().getStackTrace()(stackTraceOffset))
}

trait HasDefinedInSourceCodeAt {
  def definedInSourceCodeAt: DefinedInSourceCodeAt
}

class DefinedInSourceCodeAt(val st: StackTraceElement) {
  override lazy val toString = {
    val s = st.toString
    val i = s.lastIndexOf("(")
    s.substring(i)
  }

  override def equals(other: Any) = other match {
    case d: DefinedInSourceCodeAt => d.toString == toString && d.getClass == getClass
    case _ => false
  }

  override def hashCode = toString.hashCode
}

trait EngineComponent[P, R] extends HasDefinedInSourceCodeAt {
  def definedInSourceCodeAt: DefinedInSourceCodeAt

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
        case _ => throw new IllegalStateException("Cannot modified child")
      }

      def lastAddedChild(h: UseCase[P, R]) = h.components.headOption

      def childToHolder(child: EngineComponent[P, R]): UseCase[P, R] = child.asInstanceOf[UseCase[P, R]]

      def badChild(topParent: UseCase[P, R], child: EngineComponent[P, R], exception: Exception): UseCase[P, R] = topParent.errors.get(child) match {
        case Some(e) => topParent
        case _ => topParent.copy(errors = topParent.errors + (child -> exception))
      }
    }
}

case class UseCase[P, R](title: String, components: List[EngineComponent[P, R]] = List(), comment: Option[String], definedInSourceCodeAt: DefinedInSourceCodeAt, errors: ListMap[EngineComponent[P, R], Exception]) extends EngineComponent[P, R] with ToSummary with HasComment {
  def allScenarios = components.reverse.flatMap(_.allScenarios)

  override def toSummary(displayProcessor: DisplayProcessor): String = s"UseCase($title${comment.map(c => s",$c").getOrElse("")})"
}
