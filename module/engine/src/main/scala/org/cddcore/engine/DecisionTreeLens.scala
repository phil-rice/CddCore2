package org.cddcore.engine

import org.cddcore.enginecomponents.Scenario
import org.cddcore.utilities.Lens

/**
  * Created by User on 6/5/2016.
  */
object DecisionTreeLens {
  def identityLens[X] = Lens[X, X](x => x, (x1, xnew) => xnew)

  def concScenariosLens[P, R] = Lens[ConclusionNode[P, R], List[Scenario[P, R]]](_.scenarios, (c, s) => c.copy(scenarios = s))

  def dtToConc[P, R] = Lens[DecisionTree[P, R], ConclusionNode[P, R]](_.asInstanceOf[ConclusionNode[P, R]], (dt, c) => c)

  def dtToDN[P, R] = Lens[DecisionTree[P, R], DecisionNode[P, R]](_.asInstanceOf[DecisionNode[P, R]], (dt, dn) => dn)

  def dtFalse[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.falseNode, (dn, n) => dn.copy(falseNode = n))

  def dtTrue[P, R] = Lens[DecisionNode[P, R], DecisionTree[P, R]](_.trueNode, (dn, n) => dn.copy(trueNode = n))
}
