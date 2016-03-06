package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.{DisplayProcessor, ToSummary}

trait ScenarioAssertion[P, R] extends ToSummary{
  def valid(p: P, r: R): Boolean
  def prettyDescription: String
}

class UnknownAssertion[P, R] extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = true
  def prettyDescription: String = "<Unknown>"

  override def toSummary(displayProcessor: DisplayProcessor): String = "<Unknown>"
}

case class EqualsAssertion[P, R](expected: R) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = r == expected
  def prettyDescription: String = s"produces $expected"

  override def toSummary(dp: DisplayProcessor): String = {
    println(s"In equals assertion. DP is $dp. Result is ${dp(expected)}")
    s"produces ${dp(expected)}"
  }
}

case class ResultAssertion[P, R](fn: R => Boolean) extends ScenarioAssertion[P, R] {
  def valid(p: P, r: R) = fn(r)
  def prettyDescription: String = s"produces something where ${fn}"

  override def toSummary(displayProcessor: DisplayProcessor): String =s"produces something"
}

//case class ArbitaryAssertion[P, R](fn: (P, R) => Boolean) extends ScenarioAssertion[P, R] {
//  def valid(p: P, r: R) = fn(p, r)
//  def prettyDescription: String = s"produces something where ${fn}"
//}