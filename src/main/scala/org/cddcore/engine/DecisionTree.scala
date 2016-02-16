package org.cddcore.engine

import org.cddcore.utilities.Lens


object DecisionTree {
  def empty[P, R] = new UndecidedNode[P, R]

  private def addScenarioToConclusionNode[P, R](cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario, s) match {
      case (_: SituationAndResultScenario[P, R], _: SituationAndResultScenario[P, R]) | (_, _: SituationAndResultScenario[P, R]) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (_: SituationAndResultScenario[P, R], _) =>
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
  }

  def apply[P, R](scenarios: Seq[Scenario[P, R]]): DecisionTree[P, R] = scenarios match {
    case h :: tail => tail.foldLeft[DecisionTree[P, R]](ConclusionNode(h)) {
      (dt, s) => apply(dt, s)
    }


  }

}

trait DecisionTree[P, R] {
  def becauseIsTrue(p: P): Boolean

  def result(p: P): R

  //  def identityLens[X] = Lens[X, X](x => x, (x1, xnew) => xnew)
  //
  //  def concScenariosLens = Lens[ConclusionNode[P, R], List[Scenario[P, R]]](_.scenarios, (c, s) => c.copy(scenarios = s))
  //
  //  val dtToConc = Lens[DecisionTree[P, R], ConclusionNode[P, R]](_.asInstanceOf[ConclusionNode[P, R]], (dt, c) => c)
  //
  //  val dtToDN = Lens[DecisionTree[P, R], DecisionNode[P, R]](_.asInstanceOf[DecisionNode[P, R]], (dt, dn) => dn)
  //
  //  val dtFalse = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(falseNode = n))
  //
  //  val dtTrue = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(trueNode = n))

}

class UndecidedNode[P, R] extends DecisionTree[P, R] {
  def becauseIsTrue(p: P) = true

  def result(p: P) = throw new RuntimeException
}

trait DecisionTreeWithPartialFunction[P, R] extends DecisionTree[P, R] {
  def fn: PartialFunctionWithDescription[P, R]

  def becauseIsTrue(p: P) = fn.isDefinedAt(p)

  def result(p: P) = fn(p)

}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTreeWithPartialFunction[P, R] {
  def fn = mainScenario
}

case class DecisionNode[P, R](fn: PartialFunctionWithDescription[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R]) extends DecisionTreeWithPartialFunction[P, R] {
}

