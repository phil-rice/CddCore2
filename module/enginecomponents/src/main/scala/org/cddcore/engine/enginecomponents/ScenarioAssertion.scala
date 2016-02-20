package org.cddcore.engine.enginecomponents

trait ScenarioAssertion[P, R] {
  def valid(p: P, r: R): Boolean
}

class UnknownAssertion[P, R] extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = true
}

case class EqualsAssertion[P, R](expected: R) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = r == expected
}

case class ResultAssertion[P, R](fn: R => Boolean) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = fn(r)
}

case class ArbitaryAssertion[P, R](fn: (P, R) => Boolean) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = fn(p, r)
}