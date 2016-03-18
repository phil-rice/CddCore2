package org.cddcore.engine

import org.cddcore.engine.enginecomponents.{HasComment, EngineComponent, Scenario, UseCase}
import org.cddcore.utilities._

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

//class EngineBuilder[P, R](initialTitle: String, definedInSourceCodeAt: String) extends ChildLifeCycle[Scenario[P, R]] {
//  private var builder = HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]](UseCase[P, R](initialTitle, definedInSourceCodeAt = definedInSourceCodeAt))
//
//  def mod(fn: HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]] => HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]]) =
//    builder = fn(builder)
//
//  def title = builder.holder.title
//
//  def created(child: Scenario[P, R]) = mod(builder => builder.addChild(child))
//
//  def modified(oldChild: Scenario[P, R], newChild: Scenario[P, R]) = mod(builder => builder.modCurrentChild(_ => newChild))
//
//  def seal {}
//
//  def allScenarios = builder.holder.allScenarios
//
//  def asUseCase = builder.holder
//
//  def last = builder.currentChild match {
//    case Some(s: Scenario[P, R]) => s.expectedOption.getOrElse(throw new NoLastException("No result specified"))
//    case None => throw new NoLastException
//  }
//
//  lazy val mocks = allScenarios.foldLeft(Map[P, R]()) { (acc, s) => s.expectedOption.fold(acc)(expected => acc + (s.situation -> expected)) }
//
//}

abstract class AbstractEngine[P, R](initialTitle: String = "Untitled", val definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt())
                                   (implicit val hierarchy: Hierarchy[UseCase[P, R], EngineComponent[P, R]], dp: DisplayProcessor)
  extends EngineComponent[P, R] with MutableHierarchyBuilderWithChildLifeCycle[UseCase[P, R], EngineComponent[P, R]] {


  def makeRootHolder = UseCase[P, R](initialTitle, definedInSourceCodeAt = definedInSourceCodeAt)

  def title: String = hierarchyBuilder.holder.title

  def title(newTitle: String): Unit =
    hierarchyBuilder = {
      val oldUseCase: UseCase[P, R] = hierarchyBuilder.holder
      val newUseCase: UseCase[P, R] = oldUseCase.copy(title = newTitle)
      new HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]](newUseCase, hierarchyBuilder.depth)(hierarchy) // And I have no idea why I couldn't just use copy....
    }

  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, None)(blockThatScenariosAreDefinedIn)

  protected def useCase(title: String, comment: String)(blockThatScenariosAreDefinedIn: => Unit) = useCasePrim(title, Some(comment))(blockThatScenariosAreDefinedIn)

  private def useCasePrim(title: String, comment: Option[String])(blockThatScenariosAreDefinedIn: => Unit) =
    addParentChildrenDefinedInBlock(UseCase[P, R](title, comment = comment, definedInSourceCodeAt = EngineComponent.definedInSourceCodeAt(6)))(blockThatScenariosAreDefinedIn)

  def mocks = allScenarios.foldLeft(Map[P, R]()) { (acc, s) => s.expectedOption.fold(acc)(expected => acc + (s.situation -> expected)) }

  lazy val decisionTree = {
    seal
    val actualMocks = mocks
    val ss: List[Scenario[P, R]] = allScenarios.toList
    val noMocks = allScenarios.flatMap { s => try {
      s(actualMocks, s.situation)
      Nil
    } catch {
      case e: MockValueNotFoundException => List((s, e.p))
    }
    }.toList

    if (!noMocks.isEmpty)
      throw new RecursiveScenariosWithoutMocksException(definedInSourceCodeAt, actualMocks.keySet, noMocks)
    DecisionTree(mocks, ss)
  }

  def something = Scenario.something[R]

  def apply(p: P): R = decisionTree(apply, p)

  def allScenarios: TraversableOnce[Scenario[P, R]] = asUseCase.allScenarios

  def asUseCase = hierarchyBuilder.holder


  def validate = DecisionTree.validate(mocks, decisionTree) match {
    case Nil =>
    case list => throw new ValidationException(list)
  }

  def ifThenString = {
    s"Engine()"
  }

  def last = hierarchyBuilder.currentChild match {
    case Some(s: Scenario[P, R]) => s.expectedOption.getOrElse(throw new NoLastException("No result specified"))
    case None => throw new NoLastException
  }

}

class Engine[P, R](initialTitle: String = "Untitled", definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt())(implicit dp: DisplayProcessor) extends AbstractEngine[P, R](initialTitle, definedInSourceCodeAt) with Function[P, R] {
}

class Engine2[P1, P2, R](initialTitle: String = "Untitled", definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt())(implicit val dp: DisplayProcessor) extends AbstractEngine[(P1, P2), R](initialTitle, definedInSourceCodeAt) with Function2[P1, P2, R] {
  def apply(p1: P1, p2: P2): R = apply((p1, p2))
}

class Engine3[P1, P2, P3, R](initialTitle: String = "Untitled", definedInSourceCodeAt: String = EngineComponent.definedInSourceCodeAt())(implicit val dp: DisplayProcessor) extends AbstractEngine[(P1, P2, P3), R](initialTitle, definedInSourceCodeAt) with Function3[P1, P2, P3, R] {
  def apply(p1: P1, p2: P2, p3: P3): R = apply((p1, p2, p3))
}

class FoldLeftEngine[Acc, V](implicit dp: DisplayProcessor) extends Engine2[Acc, V, Acc]



