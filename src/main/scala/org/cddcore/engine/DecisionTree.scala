package org.cddcore.engine

object DecisionTree {
  def apply[P, R](scenarios: Seq[Scenario[P, R]]): DecisionTree[P, R] = ConclusionNode[P, R](scenarios)
}


trait DecisionTree[P, R] {

}

case class ConclusionNode[P, R](scenarios: Seq[Scenario[P, R]]) extends DecisionTree[P, R] {

}

case class DecisionNode[P, R](fn: PartialFunctionWithDescription[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R])

