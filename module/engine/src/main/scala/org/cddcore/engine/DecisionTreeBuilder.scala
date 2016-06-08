package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.{ChildLifeCycle, DisplayProcessor, Monitor}


object DecisionTreeBuilder {
  implicit def scenarioToConditionNode[P, R](s: Scenario[P, R]) = ConclusionNode[P, R](s)

  implicit def scenarioTuple2ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2))

  implicit def scenarioTuple3ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2, s._3))

  implicit def scenarioToDecisionNodeAndIfTrue[P, R](s: Scenario[P, R]) = new DecisionNodePimper(scenarioToConditionNode(s))
}

class DecisionTreeBuilder[P, R](mockEngine: P => R)(implicit monitor: Monitor, dp: DisplayProcessor, childLifeCycle: ChildLifeCycle[EngineComponent[P, R]]) {

  def withException(dt: DecisionTree[P, R], s: Scenario[P, R], e: Exception) = {
    childLifeCycle.childHasException(s, e)
    dt
  }

  def isDefinedAtMainNew(main: Scenario[P, R], newS: Scenario[P, R]) = try {
    main.isDefinedAt(mockEngine, newS.situation)
  } catch {
    case e: Exception => throw new ScenarioCausesExceptionInOtherScenariosWhenClause[P, R](newS, main, e)
  }

  def isDefinedAtNewMain(newS: Scenario[P, R], main: Scenario[P, R]) = try {
    newS.isDefinedAt(mockEngine, main.situation)
  } catch {
    case e: Exception => throw new ScenarioCausesExceptionInOtherScenariosWhenClause[P, R](newS, main, e)
  }

  private def addScenarioToConclusionNode(cn: ConclusionNode[P, R], s: Scenario[P, R]): DecisionTree[P, R] = {
    (cn.mainScenario.reason.hasWhy, s.reason.hasWhy) match {
      case (_, false) => cn.copy(scenarios = cn.scenarios :+ s)
      case (false, true) => {
        if (cn.scenarios.forall(os => isDefinedAtMainNew(s, os)))
          cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
        else
          makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
      }
      case _ => (cn.mainScenario.canMerge, s.canMerge) match {
        case (true, true) =>
          cn.copy(scenarios = cn.scenarios :+ s)
        case x => withException(cn, s, new AddingWithRedundantReason(s, cn.mainScenario))
      }
    }
  }

  def makeDecisionNode(s: Scenario[P, R], trueAnchor: Scenario[P, R], falseAnchor: Scenario[P, R], otherScenarios: List[Scenario[P, R]]): DecisionTree[P, R] = {
    val (trueSituations, falseSituations) = otherScenarios.partition(os => s.isDefinedAt(mockEngine, os.situation))
    DecisionNode(s, trueNode = ConclusionNode[P, R](trueAnchor, trueSituations), falseNode = ConclusionNode(falseAnchor, falseSituations))
  }

  def addOne(dt: DecisionTree[P, R], s: Scenario[P, R]): DecisionTree[P, R] = try {
    monitor[DecisionTree[P, R]](s"DecisionTreeBuilder.addOne($s)", {
      dt.lensFor(mockEngine, s).transform(dt, { case cn: ConclusionNode[P, R] =>
        monitor(s"conditionNode $cn", {
          if (isDefinedAtMainNew(cn.mainScenario, s)) {
            monitor("cn.isDefinedAt(s)")
            val actual = cn.mainScenario(mockEngine, s.situation)
            monitor(s"actual value was $actual")
            if (s.assertion.valid(s.situation, actual))
              monitor("Situation comes to correct conclusion in this condition node", addScenarioToConclusionNode(cn, s))
            else if (isDefinedAtNewMain(s, cn.mainScenario)) {
              monitor("s.isDefinedAt(cn) so the scenario cannot be added")
              withException(cn, s, CannotAddScenarioException(s, cn.mainScenario, actual))
            } else
              monitor("Situation comes to wrong conclusion in this condition node", makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios))
          } else
            monitor("cn.isNOTDefinedAt(situation)", makeDecisionNode(cn.mainScenario, trueAnchor = cn.mainScenario, falseAnchor = s, otherScenarios = cn.scenarios))
        })
      })
    })
  } catch {
    case e: Exception => withException(dt, s, e)
  }
}

class DecisionNodePimper[P, R](dt: DecisionTree[P, R]) {
  def ifTrue(trueNode: DecisionTree[P, R]) = DecisionTreeNodeAndIfTrue(dt, trueNode)

  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = dt, falseNode = falseNode)
}
