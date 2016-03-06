package org.cddcore.engine

import org.cddcore.engine.enginecomponents._
import org.cddcore.utilities.{DisplayProcessor, Lens, Monitor}

import scala.language.implicitConversions


class ScenarioValidationChecker[P, R](val fn: (P => R, DecisionTree[P, R], Scenario[P, R]) => Option[String])

class ConclusionNodeValidationChecker[P, R](val fn: (P => R, DecisionTree[P, R], ConclusionNode[P, R], Scenario[P, R]) => Option[String])


trait DecisionTreeValidator {

  object ValidationIssues {
    val lensReportsWrongScenario = "Lens reports wrong scenario"
    val scenarioIsNotDefinedAtConclusionNode = "Scenario not defined at conclusion node"
    val scenarioComesToWrongConclusionInNode = "Scenario comes to wrong conclusion in this node"
    val scenarioComesToWrongConclusion = "Scenario comes to wrong conclusion"
  }

  protected def lensValidationChecker[P, R] =
    new ScenarioValidationChecker[P, R]((engine, dt, s) => if (dt.lensFor(engine, s).get(dt).allScenarios.toList.contains(s)) None else Some(ValidationIssues.lensReportsWrongScenario))

  protected def scenarioInConclusionNodeChecker[P, R] =
    new ConclusionNodeValidationChecker[P, R]((engine, dt, cn, s) =>
      if (cn.isDefinedAt(engine, s.situation)) None
      else
        Some(ValidationIssues.scenarioIsNotDefinedAtConclusionNode))

  protected def scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker[P, R] =
    new ConclusionNodeValidationChecker[P, R]((engine, dt, cn, s) => {
      //      if (cn.apply(s.situation) == s.expected) None
      val result = cn.apply(engine, s.situation)
      if (s.assertion.valid(s.situation, result)) None
      else s.assertion match {
        case EqualsAssertion(_) => Some(ValidationIssues.scenarioComesToWrongConclusionInNode)
        //TODO Need tests for other assertions
      }
    })


  //TODO rename this to cover asserions
  protected def scenarioComesToCorrectAnswer[P, R] = new ScenarioValidationChecker[P, R]((engine, dt, s) => {
    val actual = dt.apply(engine, s.situation)
    if (s.assertion.valid(s.situation, actual)) None
    else s.assertion match {
      case EqualsAssertion(_) =>
        Some(ValidationIssues.scenarioComesToWrongConclusion + "\nActual value is " + actual + "\n")
    }
  })

  protected def scenarioValidators[P, R] = List[ScenarioValidationChecker[P, R]](lensValidationChecker, scenarioComesToCorrectAnswer)

  protected def conclusionNodeValidators[P, R] = List[ConclusionNodeValidationChecker[P, R]](scenarioInConclusionNodeChecker, scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker)

  protected def validateScenarios[P, R](engine: P => R, dt: DecisionTree[P, R], checker: ScenarioValidationChecker[P, R]) =
    dt.allScenarios.flatMap { s => checker.fn(engine, dt, s).map(msg => ValidationReport(msg, s)) }

  protected def validateConclusionNodes[P, R](engine: P => R, dt: DecisionTree[P, R], validator: ConclusionNodeValidationChecker[P, R]): TraversableOnce[ValidationReport[P, R]] =
    dt.allConclusionNodes.flatMap(cn => cn.allScenarios.flatMap(s => validator.fn(engine, dt, cn, s).map(ValidationReport(_, s))))


  def validate[P, R](engine: P => R, dt: DecisionTree[P, R]) =
    scenarioValidators[P, R].flatMap(validateScenarios(engine, dt, _)) :::
      conclusionNodeValidators[P, R].flatMap(validateConclusionNodes(engine, dt, _))
}

object DecisionTree extends DecisionTreeValidator {

  private def addScenarioToConclusionNode[P, R](mockEngine: P => R, cn: ConclusionNode[P, R], s: Scenario[P, R]) = {
    (cn.mainScenario.reason.hasWhy, s.reason.hasWhy) match {
      case (_, false) =>
        cn.copy(scenarios = cn.scenarios :+ s)
      case (false, true) => {
        if (cn.scenarios.forall(os => s.isDefinedAt(mockEngine, os.situation)))
          cn.copy(mainScenario = s, scenarios = cn.mainScenario :: cn.scenarios)
        else
          makeDecisionNode(mockEngine, s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios)
      }
    }
  }

  def makeDecisionNode[P, R](mockEngine: P => R, s: Scenario[P, R], trueAnchor: Scenario[P, R], falseAnchor: Scenario[P, R], otherScenarios: List[Scenario[P, R]]) = {
    val (trueSituations, falseSituations) = otherScenarios.partition(os => s.isDefinedAt(mockEngine, os.situation))
    DecisionNode(s, trueNode = ConclusionNode[P, R](trueAnchor, trueSituations), falseNode = ConclusionNode(falseAnchor, falseSituations))
  }

  def addOne[P, R](mockEngine: P => R, dt: DecisionTree[P, R], s: Scenario[P, R])(implicit monitor: Monitor, dp: DisplayProcessor): DecisionTree[P, R] = {
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
                throw new CannotAddScenarioException(s, cn.mainScenario, actual)
              } else
                monitor("Situation comes to wrong conclusion in this condition node", makeDecisionNode(mockEngine, s, trueAnchor = s, falseAnchor = cn.mainScenario, otherScenarios = cn.scenarios))
            } else
              monitor("cn.isNOTDefinedAt(situation)", makeDecisionNode(mockEngine, cn.mainScenario, trueAnchor = cn.mainScenario, falseAnchor = s, otherScenarios = cn.scenarios))
          })
        })
    })
  }

  def apply[P, R](mockEngine: P => R, scenarios: Seq[Scenario[P, R]])(implicit monitor: Monitor, dp: DisplayProcessor): DecisionTree[P, R] = {
    type DT = DecisionTree[P, R]
    monitor[DT](s"DecisionTree.apply(count of Scenarios is ${scenarios.size}",{
      scenarios match {
        case h :: tail => tail.foldLeft[DecisionTree[P, R]](monitor[DT](s"Initial tree is formed from $h", ConclusionNode(h))) {
          (dt, s) => addOne(mockEngine, dt, s)
        }
      }
    })
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

trait DecisionTree[P, R] extends EngineComponent[P, R] {

  def lensFor(mockEngine: P => R, s: Scenario[P, R]): Lens[DecisionTree[P, R], DecisionTree[P, R]]

  def mainScenario: Scenario[P, R]

  def isDefinedAt(engine: P => R, p: P) = mainScenario.isDefinedAt(engine, p)

  def apply(engine: P => R, p: P): R

  def definedInSourceCodeAt: String = mainScenario.definedInSourceCodeAt

  def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]]

}

object ConclusionNode {
  def apply[P, R](s: Scenario[P, R]): ConclusionNode[P, R] = new ConclusionNode(s, List())
}

case class ConclusionNode[P, R](mainScenario: Scenario[P, R], scenarios: List[Scenario[P, R]]) extends DecisionTree[P, R] {

  import DecisionTreeLens._

  def withScenario(s: Scenario[P, R]) = copy(scenarios = s :: scenarios)

  def lensFor(mockEngine: P => R, s: Scenario[P, R]) = identityLens

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

  def allScenarios: TraversableOnce[Scenario[P, R]] = trueNode.allScenarios.toIterator ++ falseNode.allScenarios

  override def toString = s"DecisionNode(${mainScenario} ifTrue $trueNode ifFalse $falseNode"

  override def allConclusionNodes: TraversableOnce[ConclusionNode[P, R]] = trueNode.allConclusionNodes.toIterator ++ falseNode.allConclusionNodes
}