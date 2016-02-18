package org.cddcore.core.engine

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
}


case class UseCase[P, R](title: String, components: List[EngineComponent[P, R]] = List(), comment: Option[String] = None, definedInSourceCodeAt: String) extends EngineComponent[P, R] {
  def allScenarios = components.reverse.flatMap(_.allScenarios)

}
