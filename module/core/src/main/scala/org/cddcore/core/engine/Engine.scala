package org.cddcore.core.engine

import org.cddcore.core.builder.{ChildLifeCycle, HierarchyBuilder}

import scala.language.implicitConversions


trait PartialFunctionWithDescription[P, R] extends PartialFunction[P, R] {
  def becauseDescription: String

  def thenDescription: String
}


object Engine {
  implicit def toPartial[P, R](r: R): PartialFunction[P, R] = new PartialFunction[P, R] {
    override def isDefinedAt(x: P): Boolean = true

    override def apply(v1: P): R = r
  }
}

class EngineBuilder[P, R](initialTitle: String, definedInSourceCodeAt: String) extends ChildLifeCycle[Scenario[P, R]] {
  private var builder = HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]](UseCase[P, R](initialTitle, definedInSourceCodeAt = definedInSourceCodeAt))

  def mod(fn: HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]] => HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]]) =
    builder = fn(builder)

  def title = builder.holder.title

  def created(child: Scenario[P, R]) = mod(builder => builder.addChild(child))

  def modified(oldChild: Scenario[P, R], newChild: Scenario[P, R]) = mod(builder => builder.modCurrentChild(_ => newChild))

  def seal {}

  def allScenarios = builder.holder.allScenarios

  def asUseCase = builder.holder
}

class Engine[P, R](initialTitle: String = "Untitled", val definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt()) extends EngineComponent[P, R] with Function[P, R] {
  implicit val builder = new EngineBuilder[P, R](initialTitle, definedInSourceCodeAt)

  def title: String = builder.title

  def title(newTitle: String) = builder.mod(b => b.copy(holder = b.holder.copy(title = newTitle)))


  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, None)(blockThatScenariosAreDefinedIn)

  protected def useCase(title: String, comment: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, Some(comment))(blockThatScenariosAreDefinedIn)

  private def useCasePrim(title: String, comment: Option[String])(blockThatScenariosAreDefinedIn: => Unit) = {
    val first = builder.mod(b => b.addNewParent(UseCase[P, R](title, comment = comment, definedInSourceCodeAt = EngineComponent.definedInSourceCodeAt(6))))
    blockThatScenariosAreDefinedIn
    builder.mod(_.popParent)
  }

  lazy val decisionTree = {
    builder.seal
    DecisionTree(allScenarios.toSeq)
  }

  def apply(p: P): R = decisionTree(p)

  def allScenarios: TraversableOnce[Scenario[P, R]] = builder.allScenarios

  def asUseCase = builder.asUseCase

  def validate = DecisionTree.validate(decisionTree) match {
    case Nil =>
    case list => throw new ValidationException(list)
  }
}




