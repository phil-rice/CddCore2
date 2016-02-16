package org.cddcore.engine


trait PartialFunctionWithDescription[P, R] extends PartialFunction[P, R] {
  def becauseDescription: String

  def thenDescription: String
}


case class UseCase[P, R](title: String, scenarios: List[Scenario[P, R]], comment: Option[String] = None)


case class EngineBuilder[P, R](title: String, useCases: List[UseCase[P, R]] = List()) {
  def withNewUseCase(useCase: UseCase[P, R]) = copy(useCases = useCase :: useCases)

  def modifyCurrentUseCase(fn: UseCase[P, R] => UseCase[P, R]) = copy(useCases = useCases match {
    case Nil => List(fn(UseCase("Untitled", List())))
    case h :: tail => fn(h) :: tail
  })

  def allScenarios = useCases.flatMap(_.scenarios)

  lazy val tree = DecisionTree[P, R](allScenarios)

}

class NotCoveredByScenarioException(p: Any) extends Exception(s"Parameter is $p")

class ResultNotGivenException extends Exception

object Engine {
  implicit def toPartial[P, R](r: R): PartialFunction[P, R] = new PartialFunction[P, R] {
    override def isDefinedAt(x: P): Boolean = true

    override def apply(v1: P): R = r
  }
}

class Engine[P, R](initialTitle: String = "Untitled") extends Function[P, R] {
  var builder = EngineBuilder[P, R](initialTitle)

  def title: String = builder.title

  protected implicit def toPartial[P, R](r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder[P, R](p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(scenarios: Scenario[P, R]*): Unit =
    builder = builder.withNewUseCase(UseCase(title, scenarios.toList))

  protected def title(newTitle: String): Unit = builder = builder.copy(title = newTitle)

  def apply(p: P): R = builder.tree(p)

}

