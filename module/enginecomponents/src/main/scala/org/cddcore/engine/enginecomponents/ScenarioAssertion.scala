package org.cddcore.engine.enginecomponents

trait ScenarioAssertion[P, R] {
  def valid(p: P, r: R): Boolean
  def prettyDescription: String
}

class UnknownAssertion[P, R] extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = true
  def prettyDescription: String = "<Unknown>"
}

case class EqualsAssertion[P, R](expected: R) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = r == expected
  def prettyDescription: String = s"produces $expected"
}

case class ResultAssertion[P, R](fn: R => Boolean) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = fn(r)
  def prettyDescription: String = s"produces something where ${fn}"
}

//case class ArbitaryAssertion[P, R](fn: (P, R) => Boolean) extends ScenarioAssertion[P, R] {
//  def valid(p: P, r: R) = fn(p, r)
//  def prettyDescription: String = s"produces something where ${fn}"
//}