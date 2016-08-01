/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EqualsAssertion, Scenario}

case class ValidationReport[P, R](message: String, scenario: Scenario[P, R])

class ScenarioValidationChecker[P, R](val fn: (P => R, DecisionTree[P, R], Scenario[P, R]) => Option[String])

class ConclusionNodeValidationChecker[P, R](val fn: (P => R, DecisionTree[P, R], ConclusionNode[P, R], Scenario[P, R]) => List[String])


trait DecisionTreeValidator {

  object ValidationIssues {
    val scenarioInWrongConclusionNode = "Scenario is in the wrong conclusion node in the generated decision tree. This is almost certainly a CDD software error"
    val scenarioComesToWrongConclusionInNode = "Scenario comes to wrong conclusion in this node"
  }

  protected def ScenarioIsInCorrectConclusionNodeChecker[P, R] =
    new ScenarioValidationChecker[P, R]((engine, dt, s) =>
      if (dt.lensFor(engine, s).get(dt).allScenarios.toList.contains(s))
        None
      else
        Some(ValidationIssues.scenarioInWrongConclusionNode))


  protected def scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker[P, R] =
    new ConclusionNodeValidationChecker[P, R]((engine, dt, cn, s) => {
      //      if (cn.apply(s.situation) == s.expected) None
      val result = cn.apply(engine, s.situation)
      s.assertions.flatMap(assertion =>
        if (assertion.valid(s.situation, result)) None
        else
          assertion match {
            case EqualsAssertion(expected) => Some(ValidationIssues.scenarioComesToWrongConclusionInNode)
          }
      )
    })

  protected def scenarioValidators[P, R] = List[ScenarioValidationChecker[P, R]](ScenarioIsInCorrectConclusionNodeChecker)

  protected def conclusionNodeValidators[P, R] = List[ConclusionNodeValidationChecker[P, R]](scenarioComesToCorrectAnswerWhenCheckedAgainstNodeChecker)

  protected def validateScenarios[P, R](engine: P => R, dt: DecisionTree[P, R], checker: ScenarioValidationChecker[P, R]) =
    dt.allScenarios.flatMap { s => checker.fn(engine, dt, s).map(msg => ValidationReport(msg, s)) }

  protected def validateConclusionNodes[P, R](engine: P => R, dt: DecisionTree[P, R], validator: ConclusionNodeValidationChecker[P, R]): TraversableOnce[ValidationReport[P, R]] =
    dt.allConclusionNodes.flatMap(cn => cn.allScenarios.flatMap(s => validator.fn(engine, dt, cn, s).map(ValidationReport(_, s))))


  def validate[P, R](engine: P => R, dt: DecisionTree[P, R]) =
    scenarioValidators[P, R].flatMap(validateScenarios(engine, dt, _)) :::
      conclusionNodeValidators[P, R].flatMap(validateConclusionNodes(engine, dt, _))
}
