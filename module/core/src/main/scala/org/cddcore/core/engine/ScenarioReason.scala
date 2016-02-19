package org.cddcore.core.engine

sealed trait ScenarioReason[P, R] extends PartialFunction[P, R]


case class NotYetValid[P, R](scenarioDefinedAt: String) extends ScenarioReason[P, R] {
  def apply(p: P) = throw new CalculatorNotGivenException(scenarioDefinedAt)

  def isDefinedAt(p: P) = true
}

case class SimpleReason[P, R](result: R) extends ScenarioReason[P, R] {
  def apply(p: P) = result

  def isDefinedAt(x: P): Boolean = true

  override def toString = s"SimpleReason($result)"
}

trait ScenarioReasonWithWhy[P, R] extends ScenarioReason[P, R]

case class WhenReason[P, R](when: P => Boolean, result: R) extends ScenarioReasonWithWhy[P, R] {
  def apply(p: P) = result

  def isDefinedAt(p: P): Boolean = when(p)

  override def toString = s"WhenReason($result)"
}

case class BecauseReason[P, R](pf: PartialFunction[P, R]) extends ScenarioReasonWithWhy[P, R] {
  def apply(p: P) = pf(p)

  def isDefinedAt(p: P) = pf.isDefinedAt(p)

  override def toString = s"BecauseReason()"
}