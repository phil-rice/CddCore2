package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CodeHolder


sealed trait When[P] {
  def apply(p: P): Boolean
}

case class WhenAllways[P](result: Boolean) extends When[P] {
  def apply(p: P) = result
}

case class WhenFunction[P](fn: CodeHolder[P => Boolean]) extends When[P] {
  def apply(p: P) = fn.fn(p)
}

case class WhenPartialFunction[P, R](fn: CodeHolder[PartialFunction[P, R]]) extends When[P] {
  def apply(p: P) = fn.fn.isDefinedAt(p)
}

sealed trait By[P, R] {
  def apply(p: P): R
}

case class ByAllways[P, R](result: R) extends By[P, R] {
  def apply(p: P) = result
}

case class ByFunction[P, R](fn: CodeHolder[P => R]) extends By[P, R] {
  def apply(p: P) = fn.fn(p)
}

case class ByPartialFunction[P, R](fn: CodeHolder[PartialFunction[P, R]]) extends By[P, R] {
  def apply(p: P) = fn.fn(p)
}

sealed trait ScenarioReason[P, R] extends PartialFunction[P, R] {
  def prettyDescription: String
  def hasWhy: Boolean
}


case class NotYetValid[P, R](scenarioDefinedAt: String) extends ScenarioReason[P, R] {
  def hasWhy = false

  def apply(p: P) = throw new CalculatorNotGivenException(scenarioDefinedAt)

  def isDefinedAt(p: P) = true

  def prettyDescription: String = "NotYetValid"
}

case class SimpleReason[P, R](result: R) extends ScenarioReason[P, R] {
  def apply(p: P) = result

  def isDefinedAt(x: P): Boolean = true

  def hasWhy = false

  override def toString = s"SimpleReason($result)"

  def prettyDescription: String = "JustBecause"
}


case class SimpleReasonWithBy[P, R](fn: CodeHolder[P => R]) extends ScenarioReason[P, R] {
  def apply(p: P) = fn.fn(p)

  def isDefinedAt(x: P): Boolean = true

  def hasWhy = false

  override def toString = s"SimpleReasonWithBy()"

  def prettyDescription: String = toString

}


case class WhenReason[P, R](when: CodeHolder[P => Boolean], result: R) extends ScenarioReason[P, R] {
  def apply(p: P) = result

  def isDefinedAt(p: P): Boolean = when.fn(p)

  def hasWhy = true

  override def toString = s"when ${when.prettyDescription}"

  def prettyDescription: String = toString

}

case class WhenByReason[P, R](when: CodeHolder[P => Boolean], fn: CodeHolder[P => R]) extends ScenarioReason[P, R] {
  def apply(p: P) = fn.fn(p)

  def isDefinedAt(p: P): Boolean = when.fn(p)

  def hasWhy = true

  override def toString = s"when ${when.prettyDescription} by ${fn.prettyDescription}"

  def prettyDescription: String = toString
}

case class BecauseReason[P, R](pf: CodeHolder[PartialFunction[P, R]]) extends ScenarioReason[P, R] {
  def apply(p: P) = pf.fn(p)

  def isDefinedAt(p: P) = pf.fn.isDefinedAt(p)

  def hasWhy = true

  override def toString = s"because ${pf.prettyDescription}"

  def prettyDescription: String = toString
}