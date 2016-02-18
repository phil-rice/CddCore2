package org.cddcore.engine

import org.cddcore.builder.HierarchyBuilder

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
  var builder = HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]](UseCase[P, R](initialTitle, definedInSourceCodeAt = definedInSourceCodeAt))


  def title: String = builder.holder.title

  def title(newTitle: String): Unit = {
    builder = builder.copy(holder = builder.holder.copy(title = newTitle))
  }

  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, None)(blockThatScenariosAreDefinedIn)

  protected def useCase(title: String, comment: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, Some(comment))(blockThatScenariosAreDefinedIn)

  private def useCasePrim(title: String, comment: Option[String])(blockThatScenariosAreDefinedIn: => Unit): Unit = {
    builder = builder.addNewParent(UseCase(title, comment = comment, definedInSourceCodeAt = EngineComponent.definedInSourceCodeAt()))
    blockThatScenariosAreDefinedIn
    builder = builder.popParent
  }


  def apply(p: P): R = ???

  def allScenarios: TraversableOnce[Scenario[P, R]] = builder.holder.allScenarios
}




