package org.cddcore.engine

import org.cddcore.utilities.Lens


object DecisionTree {
  def empty[P, R] = new UndecidedNode[P, R]

  private def addScenarioToConclusionNode[P, R](cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario, s) match {
      case (_, _: SituationAndResultScenario[P, R]) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (_: SituationAndResultScenario[P, R], _: ScenarioWithReason[P, R]) =>
        cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
    }
  }

  def apply[P, R](dt: DecisionTree[P, R], s: Scenario[P, R]): DecisionTree[P, R] = dt match {
    case cn: ConclusionNode[P, R] =>
      if (cn.becauseIsTrue(s.situation)) {
        if (cn.mainScenario(s.situation) == s.expected)
          addScenarioToConclusionNode(cn, s)
        else if (s.isDefinedAt(cn.mainScenario.situation))
          throw new RuntimeException("Cannot find reason")
        else
          DecisionNode(s, trueNode = ConclusionNode(s), falseNode = cn)
      } else
        DecisionNode(cn.mainScenario, trueNode = cn, falseNode = ConclusionNode(s))

    case dn: DecisionNode[P, R] =>

  }

  def apply[P, R](scenarios: Seq[Scenario[P, R]]): DecisionTree[P, R] = scenarios match {
    case h :: tail => tail.foldLeft[DecisionTree[P, R]](ConclusionNode(h)) {
      (dt, s) => apply(dt, s)
    }
  }

}

object DecisionTreeBuilder {
  implicit def scenarioToConditionNode[P, R](s: Scenario[P, R]) = ConclusionNode[P,R](s)

  implicit def scenarioTuple2ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R])) = ConclusionNode[P,R](s._1, List(s._2))

  implicit def scenarioTuple3ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R], Scenario[P, R])) = ConclusionNode[P,R](s._1, List(s._2, s._3))

  implicit def scenarioToDecisionNodeAndIfTrue[P, R](s: Scenario[P, R]) = DecisionNodeBuilder(scenarioToConditionNode(s))

}

case class DecisionNodeBuilder[P, R](dt: DecisionTreeWithMainScenario[P, R]) {
  def ifTrue(trueNode: DecisionTree[P, R]) = DecisionTreeNodeAndIfTrue(dt, trueNode)

  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = dt, falseNode = falseNode)
}

case class DecisionTreeNodeAndIfTrue[P, R](dt: DecisionTreeWithMainScenario[P, R], ifTrue: DecisionTree[P, R]) {
  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = ifTrue, falseNode = falseNode)
}

trait DecisionTree[P, R] {
  def becauseIsTrue(p: P): Boolean


  def result(p: P): R

  def identityLens[X] = Lens[X, X](x => x, (x1, xnew) => xnew)

  def concScenariosLens = Lens[ConclusionNode[P, R], List[Scenario[P, R]]](_.scenarios, (c, s) => c.copy(scenarios = s))

  val dtToConc = Lens[DecisionTree[P, R], ConclusionNode[P, R]](_.asInstanceOf[ConclusionNode[P, R]], (dt, c) => c)

  val dtToDN = Lens[DecisionTree[P, R], DecisionNode[P, R]](_.asInstanceOf[DecisionNode[P, R]], (dt, dn) => dn)

  val dtFalse = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(falseNode = n))

  val dtTrue = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(trueNode = n))

  def lensFor(s: Scenario[P, R]): Lens[DecisionTree[P, R], DecisionTree[P, R]] = identityLens

}

class UndecidedNode[P, R] extends DecisionTree[P, R] {
  def becauseIsTrue(p: P) = true


  def result(p: P) = throw new RuntimeException
}

trait DecisionTreeWithMainScenario[P, R] extends DecisionTree[P, R] {
  def mainScenario: Scenario[P, R]

  def becauseIsTrue(p: P) = mainScenario.isDefinedAt(p)

  def result(p: P) = mainScenario(p)

}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTreeWithMainScenario[P, R] {
  def fn = mainScenario

  def withScenario(s: Scenario[P, R]) = copy(scenarios = s :: scenarios)
}

case class DecisionNode[P, R](mainScenario: Scenario[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R]) extends DecisionTreeWithMainScenario[P, R] {

}

