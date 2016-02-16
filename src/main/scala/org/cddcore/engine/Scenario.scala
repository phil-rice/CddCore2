package org.cddcore.engine


object Scenario {
  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)


}

class ScenarioAddedNotActuallAnException extends Exception

trait Scenario[P, R] extends PartialFunctionWithDescription[P, R] {
  def situation: P

  def expected: R

  def definedInSourceCodeAt = {
    val st = addedException.getStackTrace()(1).toString
    val i = st.lastIndexOf("(")
    st.substring(i)
  }

  def addedException: ScenarioAddedNotActuallAnException

  def becauseDescription: String = "because"

  def thenDescription: String = expected.toString

  def validate = {
    if (!isDefinedAt(situation)) throw new ReasonInvalidException(this)
    val actual = apply(situation)
    if (actual != expected) throw new WrongResultProducedException(this, actual)
  }

}

case class FromSituationScenarioBuilder[P, R](situation: P) {
  def produces(result: R) = SituationAndResultScenario[P, R](situation, result, new ScenarioAddedNotActuallAnException)
}

case class SituationAndResultScenario[P, R](situation: P, expected: R, addedException: ScenarioAddedNotActuallAnException) extends Scenario[P, R] {

  def isDefinedAt(p: P): Boolean = true

  def apply(p: P) = expected

  def because[RR >: R](why: PartialFunction[P, RR]) = {
    val result = ScenarioWithBecause[P, RR](situation, expected, why, addedException);
    result.validate
    result
  }

  def when(because: P => Boolean) = {
    val result = ScenarioWithWhen[P, R](situation, expected, because, addedException);
    result.validate;
    result
  }

  override def toString = s"Scenario($situation produces $expected)"
}

trait ScenarioWithReason[P, R] extends Scenario[P, R]

case class ScenarioWithBecause[P, R](situation: P, expected: R, because: PartialFunction[P, R], addedException: ScenarioAddedNotActuallAnException) extends ScenarioWithReason[P, R] {
  def isDefinedAt(p: P): Boolean = because.isDefinedAt(p)

  def apply(p: P) = because(p)

  override def toString = s"Scenario($situation produces $expected) because $because)"
}

case class ScenarioWithWhen[P, R](situation: P, expected: R, when: P => Boolean, addedException: ScenarioAddedNotActuallAnException) extends ScenarioWithReason[P, R] {
  def isDefinedAt(p: P): Boolean = when(p)

  def apply(p: P) = expected

  override def toString = s"Scenario($situation produces $expected) when $when)"
}

