package org.cddcore.engine

import org.cddcore.enginecomponents.{EqualsAssertion, Scenario}

case class ValidationReport[P, R](message: String, scenario: Scenario[P, R])

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
