package org.cddcore.engine

import org.cddcore.utilities.Lens


object DecisionTree {

  private def addScenarioToConclusionNode[P, R](cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario, s) match {
      case (_, _: SituationAndResultScenario[P, R]) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (_: SituationAndResultScenario[P, R], _: ScenarioWithReason[P, R]) =>
        cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
    }
  }

  def apply[P, R](dt: DecisionTree[P, R], s: Scenario[P, R]): DecisionTree[P, R] =
    dt.lensFor(s).
      mod(dt, { case cn: ConclusionNode[P, R] =>
        if (cn.isDefinedAt(s.situation)) {
          if (cn.mainScenario(s.situation) == s.expected)
            addScenarioToConclusionNode(cn, s)
          else if (s.isDefinedAt(cn.mainScenario.situation))
            throw new CannotAddScenarioException(s, cn.mainScenario)
          else
            DecisionNode(s, trueNode = ConclusionNode(s), falseNode = cn)
        } else
          DecisionNode(cn.mainScenario, trueNode = cn, falseNode = ConclusionNode(s))
      })

  def apply[P, R](scenarios: Seq[Scenario[P, R]]): DecisionTree[P, R] = scenarios match {
    case h :: tail => tail.foldLeft[DecisionTree[P, R]](ConclusionNode(h)) {
      (dt, s) => apply(dt, s)
    }
  }
}

object DecisionTreeLens {
  def identityLens[X] = Lens[X, X](x => x, (x1, xnew) => xnew)

  def concScenariosLens[P, R] = Lens[ConclusionNode[P, R], List[Scenario[P, R]]](_.scenarios, (c, s) => c.copy(scenarios = s))

  def dtToConc[P, R] = Lens[DecisionTree[P, R], ConclusionNode[P, R]](_.asInstanceOf[ConclusionNode[P, R]], (dt, c) => c)

  def dtToDN[P, R] = Lens[DecisionTree[P, R], DecisionNode[P, R]](_.asInstanceOf[DecisionNode[P, R]], (dt, dn) => dn)

  def dtFalse[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(falseNode = n))

  def dtTrue[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.trueNode, (dn, n) => dn.copy(trueNode = n))
}

object DecisionTreeBuilder {
  implicit def scenarioToConditionNode[P, R](s: Scenario[P, R]) = ConclusionNode[P, R](s)

  implicit def scenarioTuple2ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2))

  implicit def scenarioTuple3ToConditionNode[P, R](s: (Scenario[P, R], Scenario[P, R], Scenario[P, R])) = ConclusionNode[P, R](s._1, List(s._2, s._3))

  implicit def scenarioToDecisionNodeAndIfTrue[P, R](s: Scenario[P, R]) = DecisionNodeBuilder(scenarioToConditionNode(s))
}

case class DecisionNodeBuilder[P, R](dt: DecisionTree[P, R]) {
  def ifTrue(trueNode: DecisionTree[P, R]) = DecisionTreeNodeAndIfTrue(dt, trueNode)

  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = dt, falseNode = falseNode)
}

case class DecisionTreeNodeAndIfTrue[P, R](dt: DecisionTree[P, R], ifTrue: DecisionTree[P, R]) {
  def ifFalse(falseNode: DecisionTree[P, R]) = DecisionNode(dt.mainScenario, trueNode = ifTrue, falseNode = falseNode)
}

trait DecisionTree[P, R] extends PartialFunction[P,R]{

  def lensFor(s: Scenario[P, R]): Lens[DecisionTree[P, R], DecisionTree[P, R]]

  def mainScenario: Scenario[P, R]

  def isDefinedAt(p: P) = mainScenario.isDefinedAt(p)

  def apply(p: P): R
}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def withScenario(s: Scenario[P, R]) = copy(scenarios = s :: scenarios)

  def lensFor(s: Scenario[P, R]) = identityLens

  def apply(p: P) = mainScenario(p)
}

case class DecisionNode[P, R](mainScenario: Scenario[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def apply(p: P) = if (mainScenario.isDefinedAt(p)) trueNode(p) else falseNode(p)

  def lensFor(s: Scenario[P, R]) = isDefinedAt(s.situation) match {
    case true => dtToDN.andThen(dtTrue[P, R]).andThen(trueNode.lensFor(s))
    case false => dtToDN.andThen(dtFalse[P, R]).andThen(falseNode.lensFor(s))
  }
}

