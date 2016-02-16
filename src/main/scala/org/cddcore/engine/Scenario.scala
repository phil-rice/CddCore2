package org.cddcore.engine

class ReasonInvalidException(s: Scenario[_, _]) extends Exception()

class WrongReasonProducedException(s: Scenario[_, _]) extends Exception()

object Scenario {
  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)


}

trait Scenario[P, R] extends PartialFunctionWithDescription[P, R] {
  def situation: P

  def expected: R

  def becauseDescription: String = "because"

  def thenDescription: String = expected.toString

  def validate = {
    if (!isDefinedAt(situation)) throw new ReasonInvalidException(this)
    if (apply(situation) != expected) throw new WrongReasonProducedException(this)
  }

}

case class FromSituationScenarioBuilder[P, R](situation: P) {
  def produces(result: R) = SituationAndResultScenario[P, R](situation, result)
}

case class SituationAndResultScenario[P, R](situation: P, expected: R) extends Scenario[P, R] {
  def isDefinedAt(p: P): Boolean = true

  def apply(p: P) = expected

  def because(why: PartialFunction[P, R]) = {
    val result = ScenarioWithBecause[P, R](situation, expected, why);
    result.validate;
    result
  }

  def when(because: P => Boolean) = {
    val result = ScenarioWithWhen[P, R](situation, expected, because);
    result.validate;
    result
  }

  override def toString = s"Scenario($situation produces $expected)"
}

case class ScenarioWithBecause[P, R](situation: P, expected: R, because: PartialFunction[P, R]) extends Scenario[P, R] {
  def isDefinedAt(p: P): Boolean = because.isDefinedAt(p)

  def apply(p: P) = because(p)
  override def toString = s"Scenario($situation produces $expected) because $because)"
}

case class ScenarioWithWhen[P, R](situation: P, expected: R, when: P => Boolean) extends Scenario[P, R] {
  def isDefinedAt(p: P): Boolean = when(p)

  def apply(p: P) = expected
  override def toString = s"Scenario($situation produces $expected) when $when)"
}

