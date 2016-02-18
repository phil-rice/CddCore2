package org.cddcore.core.engine

import org.cddcore.core.utilities.Lens

import scala.language.implicitConversions


class ScenarioValidationChecker[P, R](val fn: (DecisionTree[P, R], Scenario[P, R]) => Option[String])

class ConclusionNodeValidationChecker[P, R](val fn: (DecisionTree[P, R], ConclusionNode[P, R], Scenario[P, R]) => Option[String])


trait DecisionTreeValidator {


  object ValidationIssues {
    val lensReportsWrongScenario = "Lens reports wrong scenario"
    val scenarioIsNotDefinedAtConclusionNode = "Scenario not defined at conclusion node"
    val scenarioComesToWrongConclusionInNode = "Scenario comes to wrong conclusion in this node"
    val scenarioComesToWrongConclusion = "Scenario comes to wrong conclusion"
  }

  protected def lensValidationChecker[P, R] = new ScenarioValidationChecker[P, R]((dt, s) => if (dt.lensFor(s).get(dt).allScenarios.toList.contains(s)) None else Some(ValidationIssues.lensReportsWrongScenario))

  protected def scenarioInConclusionNodeChecker[P, R] =
    new ConclusionNodeValidationChecker[P, R]((dt, cn, s) =>
      if (cn.isDefinedAt(s.situation)) None
      else
        Some(ValidationIssues.scenarioIsNotDefinedAtConclusionNode))

  protected def scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker[P, R] =
    new ConclusionNodeValidationChecker[P, R]((dt, cn, s) =>
      if (cn.apply(s.situation) == s.expected) None
      else
        Some(ValidationIssues.scenarioComesToWrongConclusionInNode))

  protected def scenarioComesToCorrectAnswer[P, R] = new ScenarioValidationChecker[P, R]((dt, s) =>
    if (dt.apply(s.situation) == s.expected) None
    else
      Some(ValidationIssues.scenarioComesToWrongConclusion))

  protected def scenarioValidators[P, R] = List[ScenarioValidationChecker[P, R]](lensValidationChecker, scenarioComesToCorrectAnswer)

  protected def conclusionNodeValidators[P, R] = List[ConclusionNodeValidationChecker[P, R]](scenarioInConclusionNodeChecker, scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker)

  protected def validateScenarios[P, R](dt: DecisionTree[P, R], checker: ScenarioValidationChecker[P, R]) =
    dt.allScenarios.flatMap { s => checker.fn(dt, s).map(msg => ValidationReport(msg, s)) }

  protected def validateConclusionNodes[P, R](dt: DecisionTree[P, R], validator: ConclusionNodeValidationChecker[P, R]): TraversableOnce[ValidationReport[P, R]] =
    dt.allConclusionNodes.flatMap(cn => cn.allScenarios.flatMap(s => validator.fn(dt, cn, s).map(ValidationReport(_, s))))


  def validate[P, R](dt: DecisionTree[P, R]) = scenarioValidators[P, R].flatMap(validateScenarios(dt, _)) ::: conclusionNodeValidators[P, R].flatMap(validateConclusionNodes(dt, _))
}

object DecisionTree extends DecisionTreeValidator {

  private def addScenarioToConclusionNode[P, R](cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario.reason, s.reason) match {
      case (_, _: SimpleReason[P, R]) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (_: SimpleReason[P, R], _: ScenarioReasonWithWhy[P, R]) => {
        if (cn.scenarios.forall(os => s.isDefinedAt(os.situation)))
          cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
        else
          makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
      }
    }
  }

  def makeDecisionNode[P, R](s: Scenario[P, R], trueAnchor: Scenario[P, R], falseAnchor: Scenario[P, R], otherScenarios: List[Scenario[P, R]]) = {
    val (trueSituations, falseSituations) = otherScenarios.partition(os => s.isDefinedAt(os.situation))
    DecisionNode(s, trueNode = ConclusionNode[P, R](trueAnchor, trueSituations), falseNode = ConclusionNode(falseAnchor, falseSituations))
  }

  def addOne[P, R](dt: DecisionTree[P, R], s: Scenario[P, R]): DecisionTree[P, R] =
    dt.lensFor(s).
      transform(dt, { case cn: ConclusionNode[P, R] =>
        if (cn.isDefinedAt(s.situation)) {
          if (cn.mainScenario(s.situation) == s.expected)
            addScenarioToConclusionNode(cn, s)
          else if (s.isDefinedAt(cn.mainScenario.situation))
            throw new CannotAddScenarioException(s, cn.mainScenario)
          else
            makeDecisionNode(s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
        } else
          makeDecisionNode(cn.mainScenario, trueAnchor = cn.mainScenario, falseAnchor = s, otherScenarios = cn.scenarios)
      })

  def apply[P, R](scenarios: Seq[Scenario[P, R]]): DecisionTree[P, R] = scenarios match {
    case h :: tail => tail.foldLeft[DecisionTree[P, R]](ConclusionNode(h)) {
      (dt, s) => addOne(dt, s)
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

case class ValidationReport[P, R](message: String, scenario: Scenario[P, R])

trait DecisionTree[P, R] extends EngineComponent[P, R] with PartialFunction[P, R] {

  def lensFor(s: Scenario[P, R]): Lens[DecisionTree[P, R], DecisionTree[P, R]]

  def mainScenario: Scenario[P, R]

  def isDefinedAt(p: P) = mainScenario.isDefinedAt(p)

  def apply(p: P): R

  def definedInSourceCodeAt: String = mainScenario.definedInSourceCodeAt

  def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]]

}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def withScenario(s: Scenario[P, R]) = copy(scenarios = s :: scenarios)

  def lensFor(s: Scenario[P, R]) = identityLens

  def apply(p: P) = mainScenario(p)

  def allScenarios: TraversableOnce[Scenario[P, R]] = mainScenario :: scenarios

  override def toString = s"ConcNode(${mainScenario},supportedBy=${scenarios.mkString(";")})"

  override def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]] = Seq(this)
}

case class DecisionNode[P, R](mainScenario: Scenario[P, R], falseNode: DecisionTree[P, R], trueNode: DecisionTree[P, R]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def apply(p: P) = if (mainScenario.isDefinedAt(p)) trueNode(p) else falseNode(p)

  def lensFor(s: Scenario[P, R]) = isDefinedAt(s.situation) match {
    case true => dtToDN.andThen(dtTrue[P, R]).andThen(trueNode.lensFor(s))
    case false => dtToDN.andThen(dtFalse[P, R]).andThen(falseNode.lensFor(s))
  }

  def allScenarios: TraversableOnce[Scenario[P, R]] = trueNode.allScenarios.toIterator ++ falseNode.allScenarios

  override def toString = s"DecisionNode(${mainScenario} ifTrue $trueNode ifFalse $falseNode"

  override def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]] = trueNode.allConclusionNodes.toIterator ++ falseNode.allConclusionNodes
}