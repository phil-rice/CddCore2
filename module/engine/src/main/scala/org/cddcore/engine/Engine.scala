/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents._
import org.cddcore.utilities._

import scala.collection.immutable.ListMap
import scala.language.implicitConversions


trait PartialFunctionWithDescription[P, R] extends PartialFunction[P, R] {
  def becauseDescription: String

  def thenDescription: String
}


trait EngineEvaluator {
  def apply[P, R](engine: AbstractEngine[P, R], p: P): R
}

object SimpleEngineEvaluator extends EngineEvaluator {
  def apply[P, R](engine: AbstractEngine[P, R], p: P): R = engine.evaluate(p)
}

class TraceEngineEvaluator extends EngineEvaluator {
  var childTraces = List[Trace]()

  def apply[P, R](engine: AbstractEngine[P, R], p: P): R = {
    val startTime = System.nanoTime()

    val path = engine.decisionTree.pathFor(engine.evaluate, p)
    val cn: ConclusionNode[P, R] = path.last match {
      case cn: ConclusionNode[P, R] => cn
      case x => throw new IllegalStateException(s"Somehow have a path through a decision tree that didn't end in a conclusion node: $x")
    }
    val oldChildTraces = childTraces
    childTraces = Nil
    val result = cn.apply(engine.evaluate, p)
    childTraces = oldChildTraces :+ TraceEngine(startTime, System.nanoTime() - startTime, engine, cn, p, result, childTraces)
    result
  }
}

object Engine {
  implicit def toPartial[P, R](r: R): PartialFunction[P, R] = {
    case _ => r
  }

  val threadLocalEvaluator = new ThreadLocal[EngineEvaluator] {
    override def initialValue = SimpleEngineEvaluator
  }

  def apply[P, R](engine: AbstractEngine[P, R], p: P): R = threadLocalEvaluator.get().apply(engine, p)

  protected def callWith[P, R, E <: EngineEvaluator](engineEvaluator: E, engine: AbstractEngine[P, R], p: P) = {
    val oldValue = threadLocalEvaluator.get
    threadLocalEvaluator.set(engineEvaluator)
    try {
      (engineEvaluator(engine, p), engineEvaluator)
    } finally {
      threadLocalEvaluator.set(oldValue)
    }
  }

  //Sometimes the list may have multiple items in it: most often when called the first time. This is often because the building of decision trees causes other engines to be called if they are referenced by this engine.
  // The last item in the list is the trace item corresponding to the call
  def trace[P, R](engine: AbstractEngine[P, R], p: P): (R, List[Trace]) = {
    val result = threadLocalEvaluator.get match {
      case SimpleEngineEvaluator => {
        val (result, traceEngineEvaluator) = callWith(new TraceEngineEvaluator, engine, p)
        (result, traceEngineEvaluator.childTraces)
      }
      case e: TraceEngineEvaluator => throw new IllegalStateException("Cannot currently call trace when already tracing")
    }
    result
  }
}

abstract class AbstractEngine[P, R](initialTitle: String = "Untitled", val references: List[Reference] = List(), val definedInSourceCodeAt: DefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt())
                                   (implicit val hierarchy: Hierarchy[UseCase[P, R], EngineComponent[P, R]], dp: DisplayProcessor)
  extends EngineComponent[P, R] with MutableHierarchyBuilderWithChildLifeCycle[UseCase[P, R], EngineComponent[P, R]] with ToSummary {

  def allUseCases = allUseCasesPrim(asUseCase)

  private def allUseCasesPrim(uc: UseCase[P, R]): List[UseCase[P, R]] = uc.components.flatMap { case uc: UseCase[P, R] => uc :: allUseCasesPrim(uc); case _ => Nil }

  def allReferences: Seq[Reference] = references ++ allUseCases.flatMap(_.references) ++ allScenarios.flatMap(_.references)

  def allDocuments = allReferences.map(_.document).distinct

  def errors: ListMap[EngineComponent[P, R], Exception] = hierarchyBuilder.holder.errors

  def postSealMessage = "Cannot modify the engine after it has been constructed"

  def makeRootHolder = UseCase[P, R](initialTitle, comment = None, definedInSourceCodeAt = definedInSourceCodeAt, rawErrors = ListMap(), references = List())

  def title: String = hierarchyBuilder.holder.title

  def title(newTitle: String): Unit =
    hierarchyBuilder = {
      val oldUseCase: UseCase[P, R] = hierarchyBuilder.holder
      val newUseCase: UseCase[P, R] = oldUseCase.copy(title = newTitle)
      new HierarchyBuilder[UseCase[P, R], EngineComponent[P, R]](newUseCase, hierarchyBuilder.depth)(hierarchy) // And I have no idea why I couldn't just use copy....
    }

  protected implicit def toPartial(r: R): PartialFunction[P, R] = Engine.toPartial(r)

  implicit def pToScenarioBuilder(p: P) = Scenario.pToScenarioBuilder[P, R](p)

  protected def useCase(title: String)(blockThatScenariosAreDefinedIn: => Unit): Unit = useCase(title, None, List())(blockThatScenariosAreDefinedIn)

  protected def useCase(title: String, comment: String, references: List[Reference] = List())(blockThatScenariosAreDefinedIn: => Unit): Unit = useCase(title, Some(comment), references)(blockThatScenariosAreDefinedIn)

  private def useCase(title: String, comment: Option[String], references: List[Reference])(blockThatScenariosAreDefinedIn: => Unit): Unit =
    addParentChildrenDefinedInBlock(UseCase[P, R](title, comment = comment, definedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt(7), rawErrors = ListMap(), references = references))(blockThatScenariosAreDefinedIn)

  private def calculateMocksAndNoMocks: (Map[P, R], List[(Scenario[P, R], P)]) = {
    val mocks: Map[P, R] = allScenarios.foldLeft(Map[P, R]()) { (acc, s) => s.expectedOption.fold(acc)(expected => acc + (s.situation -> expected)) }
    def evalMock(p: P) = mocks.getOrElse(p, throw new MockValueNotFoundException(p))
    val noMocks = allScenarios.toList.flatMap {
      s: Scenario[P, R] => try {
        s(evalMock, s.situation)
        Nil
      } catch {
        case e: MockValueNotFoundException[P] => List((s, e.p))
        case e: CalculatorNotGivenException => childHasException(s, e); Nil
      }
    }
    (mocks, noMocks)
  }

  protected lazy val rawMocks: Map[P, R] = {
    val (result, noMocks) = calculateMocksAndNoMocks
    noMocks.foreach { case (s, p) => childHasException(s, new MockValueNotFoundException(p)) }
    result
  }

  def mocks(p: P): R = rawMocks.getOrElse(p, throw new MockValueNotFoundException(p))

  def buildDecisionTree = {
    val ss: List[Scenario[P, R]] = allScenarios.toList
    val mocks = rawMocks
    val initialErrors = hierarchyBuilder.holder.errors
    val result = DecisionTree[P, R](mocks, ss, initialErrors)
    seal
    result
  }

  lazy val decisionTree = buildDecisionTree

  def something = Scenario.something[R]

  def evaluate(p: P): R = if (hierarchyBuilder.holder.errors.isEmpty) decisionTree(evaluate, p) else throw hierarchyBuilder.holder.errors.head._2

  def allScenarios: TraversableOnce[Scenario[P, R]] = asUseCase.allScenarios

  def asUseCase = hierarchyBuilder.holder

  protected def merge = Merge

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
    case x => throw new IllegalStateException(s"Somehow called last, and the last child wasn't a scenario, instead it was ${x.getClass}\n$x")
  }

  def summary(implicit displayProcessor: DisplayProcessor): String = title
}

class Engine[P, R](initialTitle: String = "Untitled", references: List[Reference] = List(), definedInSourceCodeAt: DefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt())(implicit dp: DisplayProcessor) extends AbstractEngine[P, R](initialTitle, references, definedInSourceCodeAt) with Function[P, R] {

  def apply(p: P): R = Engine(this, p)
}

class Engine2[P1, P2, R](initialTitle: String = "Untitled", references: List[Reference] = List(), definedInSourceCodeAt: DefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt())(implicit val dp: DisplayProcessor) extends AbstractEngine[(P1, P2), R](initialTitle, references, definedInSourceCodeAt) with Function2[P1, P2, R] {
  def apply(p1: P1, p2: P2): R = Engine(this, (p1, p2))
}

class Engine3[P1, P2, P3, R](initialTitle: String = "Untitled", references: List[Reference] = List(), definedInSourceCodeAt: DefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt())(implicit val dp: DisplayProcessor) extends AbstractEngine[(P1, P2, P3), R](initialTitle, references, definedInSourceCodeAt) with Function3[P1, P2, P3, R] {
  def apply(p1: P1, p2: P2, p3: P3): R = Engine(this, (p1, p2, p3))
}

class FoldLeftEngine[Acc, V](implicit dp: DisplayProcessor) extends Engine2[Acc, V, Acc]



