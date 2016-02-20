package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CodeHolder

sealed trait ScenarioReason[P, R] extends PartialFunction[P, R] {
  def prettyDescription: String
}


case class NotYetValid[P, R](scenarioDefinedAt: String) extends ScenarioReason[P, R] {
  def apply(p: P) = throw new CalculatorNotGivenException(scenarioDefinedAt)

  def isDefinedAt(p: P) = true

  def prettyDescription: String = "NotYetValid"
}

trait ScenarioReasonWithoutWhy[P, R] extends ScenarioReason[P, R]

case class SimpleReason[P, R](result: R) extends ScenarioReasonWithoutWhy[P, R] {
  def apply(p: P) = result

  def isDefinedAt(x: P): Boolean = true

  override def toString = s"SimpleReason($result)"

  def prettyDescription: String = "JustBecause"
}

case class SimpleReasonWithBy[P, R](fn: P => R) extends ScenarioReasonWithoutWhy[P, R] {
  def apply(p: P) = fn(p)

  def isDefinedAt(x: P): Boolean = true

  override def toString = s"SimpleReasonWithBy()"

  def prettyDescription: String = toString

}


trait ScenarioReasonWithWhy[P, R] extends ScenarioReason[P, R]

case class WhenReason[P, R](when: CodeHolder[P => Boolean], result: R) extends ScenarioReasonWithWhy[P, R] {
  def apply(p: P) = result

  def isDefinedAt(p: P): Boolean = when.fn(p)

  override def toString = s"when ${when.prettyDescription}"

  def prettyDescription: String = toString

}

case class WhenByReason[P, R](when: CodeHolder[P => Boolean], fn: P => R) extends ScenarioReasonWithWhy[P, R] {
  def apply(p: P) = fn(p)

  def isDefinedAt(p: P): Boolean = when.fn(p)

  override def toString = s"when ${when.prettyDescription} by ${fn}"

  def prettyDescription: String = toString
}

case class BecauseReason[P, R](pf: CodeHolder[PartialFunction[P, R]]) extends ScenarioReasonWithWhy[P, R] {
  def apply(p: P) = pf.fn(p)

  def isDefinedAt(p: P) = pf.fn.isDefinedAt(p)

  override def toString = s"because ${pf.prettyDescription}"

  def prettyDescription: String = toString
}