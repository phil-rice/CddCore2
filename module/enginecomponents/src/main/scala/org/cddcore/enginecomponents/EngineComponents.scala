/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
        case oldChild :: tail =>
          val newChild = fn(oldChild)
          val errors = h.rawErrors
          val newErrors = errors.get(oldChild).fold(errors)(e => errors - oldChild + (newChild -> e))
          h.copy(components = fn(oldChild) :: tail, rawErrors = newErrors)
        case _ => throw new IllegalStateException("Cannot modify child")
      }

      def lastAddedChild(h: UseCase[P, R]) = h.components.headOption

      def childToHolder(child: EngineComponent[P, R]): UseCase[P, R] = child.asInstanceOf[UseCase[P, R]]

      def badChild(parent: UseCase[P, R], child: EngineComponent[P, R], exception: Exception): UseCase[P, R] = parent.rawErrors.get(child) match {
        case Some(e) => parent
        case _ => parent.copy(rawErrors = parent.rawErrors + (child -> exception))
      }
    }
}

case class UseCase[P, R](title: String, components: List[EngineComponent[P, R]] = List(), comment: Option[String],
                         definedInSourceCodeAt: DefinedInSourceCodeAt, rawErrors: ListMap[EngineComponent[P, R], Exception], references: List[Reference]) extends EngineComponent[P, R] with ToSummary with HasComment {
  lazy val allScenarios = components.reverse.flatMap(_.allScenarios)
  lazy val errors: ListMap[EngineComponent[P, R], Exception] = components.collect { case uc: UseCase[P, R] => uc.errors }.fold(rawErrors)((acc, errors) => acc ++ errors)

  override def summary(implicit displayProcessor: DisplayProcessor): String = s"UseCase($title${comment.map(c => s",$c").getOrElse("")})"
}
