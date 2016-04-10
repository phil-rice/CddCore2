/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

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
