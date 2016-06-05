/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents._
import org.cddcore.utilities.{ChildLifeCycle, DisplayProcessor, Lens, Monitor}

import scala.language.implicitConversions

object DecisionTree extends DecisionTreeValidator {

  private def addScenarioToConclusionNode[P, R](mockEngine: P => R, cn: ConclusionNode[P, R], s: Scenario[P, R])(implicit childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]) = {
    (cn.mainScenario.reason.hasWhy, s.reason.hasWhy) match {
      case (_, false) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (false, true) => {
        if (cn.scenarios.forall(os => s.isDefinedAt(mockEngine, os.situation)))
          cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
        else
          makeDecisionNode(mockEngine, s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
      }
      case _ =>
        childLifeCycle.childHasException(s, new IllegalStateException("Both main scenario and S have a reason, so I should not be adding the scenario to the main conclusion. "))
        cn
    }
  }

  def makeDecisionNode[P, R](mockEngine: P => R, s: Scenario[P, R], trueAnchor: Scenario[P, R], falseAnchor: Scenario[P, R], otherScenarios: List[Scenario[P, R]]) = {
    val (trueSituations, falseSituations) = otherScenarios.partition(os => s.isDefinedAt(mockEngine, os.situation))
    DecisionNode(s, trueNode = ConclusionNode[P, R](trueAnchor, trueSituations), falseNode = ConclusionNode(falseAnchor, falseSituations))
  }

  def addOne[P, R](mockEngine: P => R, dt: DecisionTree[P, R], s: Scenario[P, R])
                  (implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]):
  DecisionTree[P, R] = {
    monitor(s"DecisionTree.addOne($s)", {
      dt.lensFor(mockEngine, s).
        transform(dt, { case cn: ConclusionNode[P, R] =>
          monitor(s"conditionNode $cn", {
            if (cn.isDefinedAt(mockEngine, s.situation)) {
              monitor("cn.isDefinedAt(s)")
              val actual = cn.mainScenario(mockEngine, s.situation)
              monitor(s"actual value was $actual")
              if (s.assertion.valid(s.situation, actual))
                monitor("Situation comes to correct conclusion in this condition node", addScenarioToConclusionNode(mockEngine, cn, s))
              else if (s.isDefinedAt(mockEngine, cn.mainScenario.situation)) {
                monitor("s.isDefinedAt(cn) so the scenario cannot be added")
                childLifeCycle.childHasException(s, new CannotAddScenarioException(s, cn.mainScenario, actual))
                cn
              } else
                monitor("Situation comes to wrong conclusion in this condition node", makeDecisionNode(mockEngine, s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios))
            } else
              monitor("cn.isNOTDefinedAt(situation)", makeDecisionNode(mockEngine, cn.mainScenario, trueAnchor = cn.mainScenario, falseAnchor = s, otherScenarios = cn.scenarios))
          })
        })
    })
  }

  def apply[P, R](mockEngine: P => R, scenarios: Seq[Scenario[P, R]], errors: Map[EngineComponent[P, R], Exception] = Map[EngineComponent[P, R], Exception]())
                 (implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]): DecisionTree[P, R] = {
    type DT = DecisionTree[P, R]
    monitor[DT](s"DecisionTree.apply(count of Scenarios is ${scenarios.size}", {
      scenarios.filter(!errors.contains(_)) match {
        case h :: tail => tail.foldLeft[DecisionTree[P, R]](monitor[DT](s"Initial tree is formed from $h", ConclusionNode(h))) {
          (dt, s) => addOne(mockEngine, dt, s)
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

  def apply(engine: P => R, p: P) = mainScenario(engine, p)

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
