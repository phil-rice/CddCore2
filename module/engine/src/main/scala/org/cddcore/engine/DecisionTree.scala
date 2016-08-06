/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents._
import org.cddcore.utilities.{ChildLifeCycle, DisplayProcessor, Lens, Monitor}

import scala.language.implicitConversions

object DecisionTree extends DecisionTreeValidator {
  //  def addOne[P, R](mockEngine: P => R, dt: DecisionTree[P, R], s: Scenario[P, R])
  //                  (implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]) = DecisionTreeBuilder.addOne(mockEngine, dt, s)

  def apply[P, R](mockEngine: P => R, scenarios: Seq[Scenario[P, R]], errors: Map[EngineComponent[P, R], Exception] = Map[EngineComponent[P, R], Exception]())
                 (implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]): DecisionTree[P, R] = {
    val builder = new DecisionTreeBuilder[P, R](mockEngine)
    type DT = DecisionTree[P, R]
    monitor[DT](s"DecisionTree.apply(count of Scenarios is ${scenarios.size}", {
      scenarios.filter(!errors.contains(_)) match {
        case h :: tail => tail.foldLeft[DecisionTree[P, R]](monitor[DT](s"Initial tree is formed from $h", ConclusionNode(h))) {
          (dt, s) => builder.addOne(dt, s)
        }
        case _ => EmptyDecisionTree.asInstanceOf[DT]
      }
    })

  }

}


case class DecisionTreeNodeAndIfTrue[P, R](dt: DecisionTree[P, R], ifTrue: DecisionTree[P, R]) {
  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = ifTrue, falseNode = falseNode)
}


trait DecisionTree[P, R] extends EngineComponent[P, R] {

  def lensFor(mockEngine: P => R, s: Scenario[P, R]): Lens[DecisionTree[P, R], DecisionTree[P, R]]

  def pathFor(mockEngine: P => R, situation: P): List[DecisionTree[P, R]]

  def conclusionNodeFor(mockEngine: P => R, situation: P) = pathFor(mockEngine, situation).last match {
    case cn: ConclusionNode[P, R] => cn
    case dt => throw new IllegalStateException(s"Somehow had a $dt")
  }

  def mainScenario: Scenario[P, R]

  def isDefinedAt(engine: P => R, p: P) = mainScenario.isDefinedAt(engine, p)

  def apply(engine: P => R, p: P): R

  def definedInSourceCodeAt: DefinedInSourceCodeAt = mainScenario.definedInSourceCodeAt

  def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]]

}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

object EmptyDecisionTree extends DecisionTree[Nothing, Nothing] {
  def allConclusionNodes: TraversableOnce[ConclusionNode[Nothing, Nothing]] = Nil

  def lensFor(mockEngine: (Nothing) => Nothing, s: Scenario[Nothing, Nothing]): Lens[DecisionTree[Nothing, Nothing], DecisionTree[Nothing, Nothing]] = throw new IllegalStateException("An empty decision tree has no scenarios")

  def apply(engine: (Nothing) => Nothing, p: Nothing): Nothing = throw new IllegalStateException("An empty decision tree has no scenarios")

  def pathFor(mockEngine: (Nothing) => Nothing, situation: Nothing): List[DecisionTree[Nothing, Nothing]] = throw new IllegalStateException("An empty decision tree has no scenarios")

  def mainScenario: Scenario[Nothing, Nothing] = throw new IllegalStateException("An empty decision tree has no scenarios")

  def allScenarios: TraversableOnce[Scenario[Nothing, Nothing]] = Nil

  def title: String = "Empty Decision Tree: there were no valid scenarios"
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def withScenario(s: Scenario[P, R]) = copy(scenarios = s :: scenarios)

  def lensFor(mockEngine: P => R, s: Scenario[P, R]) = identityLens

  def pathFor(mockEngine: P => R, situation: P) = List(this)

  override def isDefinedAt(engine: P => R, p: P) = if (mainScenario.isDefinedAt(engine, p)) true else scenarios.exists(_.isDefinedAt(engine, p))

  def apply(engine: P => R, p: P) =
    if (mainScenario.isDefinedAt(engine, p)) mainScenario(engine, p)
    else
      scenarios.find(_.isDefinedAt(engine, p)) match {
        case Some(s) => s(engine, p)
        case _ => throw  EngineIsNotDefined(p)
      }

  def allScenarios: TraversableOnce[Scenario[P, R]] = mainScenario :: scenarios

  def title = "Conclusion: " + mainScenario

  override def toString = s"ConcNode(${mainScenario},supportedBy=${scenarios.mkString(";")})"

  override def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]] = Seq(this)
}

case class DecisionNode[P, R](mainScenario: Scenario[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def title = "Decision: " + mainScenario

  def apply(engine: P => R, p: P) = if (mainScenario.isDefinedAt(engine, p)) trueNode(engine, p) else falseNode(engine, p)

  def lensFor(mockEngine: P => R, s: Scenario[P, R]) = isDefinedAt(mockEngine, s.situation) match {
    case true => dtToDN.andThen(dtTrue[P, R]).andThen(trueNode.lensFor(mockEngine, s))
    case false => dtToDN.andThen(dtFalse[P, R]).andThen(falseNode.lensFor(mockEngine, s))
  }

  def pathFor(mockEngine: (P) => R, situation: P) = isDefinedAt(mockEngine, situation) match {
    case true => this :: trueNode.pathFor(mockEngine, situation)
    case false => this :: falseNode.pathFor(mockEngine, situation)
  }

  def allScenarios: TraversableOnce[Scenario[P, R]] = trueNode.allScenarios.toIterator ++ falseNode.allScenarios

  override def toString = s"DecisionNode(${mainScenario} ifTrue $trueNode ifFalse $falseNode"

  override def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]] = trueNode.allConclusionNodes.toIterator ++ falseNode.allConclusionNodes

}
