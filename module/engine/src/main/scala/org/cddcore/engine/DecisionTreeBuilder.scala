package org.cddcore.engine

import org.cddcore.enginecomponents.{CannotAddScenarioException, EngineComponent, Scenario, ScenarioException}
import org.cddcore.utilities.{ChildLifeCycle, DisplayProcessor, Monitor}


object DecisionTreeBuilder {
  implicit def scenarioToConditionNode[P, R](s: Scenario[P, R]) = ConclusionNode[P, R](s)

  implicit def scenarioTuple2ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2))

  implicit def scenarioTuple3ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2, s._3))

  implicit def scenarioToDecisionNodeAndIfTrue[P, R](s: Scenario[P, R]) = new DecisionNodePimper(scenarioToConditionNode(s))
}

class DecisionTreeBuilder[P, R](mockEngine: P => R)(implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]) {

  private def addScenarioToConclusionNode(cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario.reason.hasWhy, s.reason.hasWhy) match {
      case (_, false) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (false, true) => {
        if (cn.scenarios.forall(os => s.isDefinedAt(mockEngine, os.situation)))
          cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
        else
          makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
      }
      case _ =>
        childLifeCycle.childHasException(s, new IllegalStateException("Both main scenario and S have a reason, so I should not be adding the scenario to the main conclusion. "))
        cn
    }
  }

  def makeDecisionNode(s: Scenario[P, R], trueAnchor: Scenario[P, R], falseAnchor: Scenario[P, R], otherScenarios: List[Scenario[P, R]]) = {
    val (trueSituations, falseSituations) = otherScenarios.partition(os => s.isDefinedAt(mockEngine, os.situation))
    DecisionNode(s, trueNode = ConclusionNode[P, R](trueAnchor, trueSituations), falseNode = ConclusionNode(falseAnchor, falseSituations))
  }

  def addOne(dt: DecisionTree[P, R], s: Scenario[P, R]): DecisionTree[P, R] = {
    monitor(s"DecisionTree.addOne($s)", {
      dt.lensFor(mockEngine, s).
        transform(dt, { case cn: ConclusionNode[P, R] =>
          monitor(s"conditionNode $cn", {
            if (cn.isDefinedAt(mockEngine, s.situation)) {
              monitor("cn.isDefinedAt(s)")
              val actual = cn.mainScenario(mockEngine, s.situation)
              monitor(s"actual value was $actual")
              if (s.assertion.valid(s.situation, actual))
                monitor("Situation comes to correct conclusion in this condition node", addScenarioToConclusionNode(cn, s))
              else if (s.isDefinedAt(mockEngine, cn.mainScenario.situation)) {
                monitor("s.isDefinedAt(cn) so the scenario cannot be added")
                childLifeCycle.childHasException(s, new CannotAddScenarioException(s, cn.mainScenario, actual))
                cn
              } else
                monitor("Situation comes to wrong conclusion in this condition node", makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios))
            } else
              monitor("cn.isNOTDefinedAt(situation)", makeDecisionNode(cn.mainScenario, trueAnchor = cn.mainScenario, falseAnchor = s, otherScenarios = cn.scenarios))
          })
        })
    })
  }



}

class DecisionNodePimper[P, R](dt: DecisionTree[P, R]) {
  def ifTrue(trueNode: DecisionTree[P, R]) = DecisionTreeNodeAndIfTrue(dt, trueNode)

  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = dt, falseNode = falseNode)
}
