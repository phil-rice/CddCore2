package org.cddcore.engine

import org.cddcore.builder.ChildLifeCycle

import scala.language.implicitConversions


object Scenario {
  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)

  implicit def scenarioToScenarioBuilder[P, R](s: Scenario[P, R]) = ScenarioBuilder[P, R](s)
}

case class Scenario[P, R](situation: P, expected: R, reason: ScenarioReason[P, R], definedInSourceCodeAt: String) extends EngineComponent[P, R] with PartialFunction[P, R] {
  def allScenarios = Seq(this)

  def isDefinedAt(p: P) = reason.isDefinedAt(p)

  def apply(p: P) = reason(p)

  def validate = {
    if (!isDefinedAt(situation)) throw new ReasonInvalidException(this)
    val actual = apply(situation)
    if (actual != expected) throw new WrongResultProducedException(this, actual)
  }

  override def toString = s"Scenario($situation produces $expected because $reason)/$definedInSourceCodeAt"
}

case class FromSituationScenarioBuilder[P, R](situation: P) {
  def produces(result: R)(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val s = Scenario[P, R](situation, result, SimpleReason(result), EngineComponent.definedInSourceCodeAt())
    scl.created(s)
    s
  }
}

case class ScenarioBuilder[P, R](scenario: Scenario[P, R]) {
  def because(because: PartialFunction[P, R])(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val result = scenario.copy(reason = BecauseReason(because))
    scl.modified(scenario, result)
    result.validate
    result
  }

  def when(when: P => Boolean)(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val result = scenario.copy(reason = WhenReason[P, R](when, scenario.expected))
    scl.modified(scenario, result)
    result.validate
    result
  }
}

