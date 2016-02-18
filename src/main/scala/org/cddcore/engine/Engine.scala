package org.cddcore.engine

import scala.language.implicitConversions


trait PartialFunctionWithDescription[P, R] extends PartialFunction[P, R] {
  def becauseDescription: String

  def thenDescription: String
}


case class EngineBuilder[P, R](title: String, useCases: List[UseCase[P, R]] = List()) {
  def withNewUseCase(useCase: UseCase[P, R]) = copy(useCases = useCases :+ useCase)

  //  def modifyCurrentUseCase(fn: UseCase[P, R] => UseCase[P, R]) = copy(useCases = useCases match {
  //    case Nil => List(fn(UseCase("Untitled", List())))
  //    case h :: tail => fn(h) :: tail
  //  })

  def allScenarios = useCases.flatMap(_.allScenarios)

  lazy val tree = DecisionTree[P, R](allScenarios)
}


object Engine {
  implicit def toPartial[P, R](r: R): PartialFunction[P, R] = new PartialFunction[P, R] {
    override def isDefinedAt(x: P): Boolean = true

    override def apply(v1: P): R = r
  }
}

class Engine[P, R](initialTitle: String = "Untitled", val definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt()) extends EngineComponent[P, R] with Function[P, R] {
  var builder = EngineBuilder[P, R](initialTitle)

  def title: String = builder.title

  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(scenarios: Scenario[P, R]*): Unit = useCasePrim(title, None)(scenarios)

  protected def useCase(title: String, comment: String)(scenarios: Scenario[P, R]*): Unit = useCasePrim(title, Some(comment))(scenarios)


  private def useCasePrim(title: String, comment: Option[String] = None)(scenarios: Seq[Scenario[P, R]]): Unit =
    builder = builder.withNewUseCase(UseCase(title, scenarios.toList, comment, EngineComponent.definedInSourceCodeAt(3)))

  protected def title(newTitle: String): Unit = builder = builder.copy(title = newTitle)

  def apply(p: P): R = builder.tree(p)

  def allScenarios: TraversableOnce[Scenario[P, R]] = builder.allScenarios
}


case class EngineBuilder2[P, R](title: String, components: List[EngineComponent[P, R]], currentUseCase: Option[UseCase[P, R]]) {
  def allScenarios = components.flatMap(_.allScenarios)

  lazy val tree = DecisionTree[P, R](allScenarios)
}

class Engine2[P, R](initialTitle: String = "Untitled", val definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt()) extends EngineComponent[P, R] with Function[P, R] {
  var useCaseBuilder = UseCaseBuilder[P, R](UseCase[P, R](initialTitle, definedInSourceCodeAt = definedInSourceCodeAt))


  def title: String = useCaseBuilder.useCase.title

  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(blockThatScenariosAreDefinedIn: => Unit): Unit = {
    useCaseBuilder = useCaseBuilder.addNewParent(UseCase(title, definedInSourceCodeAt = EngineComponent.definedInSourceCodeAt()))
    blockThatScenariosAreDefinedIn
    useCaseBuilder.popParent
  }


  def apply(p: P): R = ???

  def allScenarios: TraversableOnce[Scenario[P, R]] = useCaseBuilder.useCase.allScenarios
}




