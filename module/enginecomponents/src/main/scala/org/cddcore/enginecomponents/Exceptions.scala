/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.DisplayProcessor

trait HasActual[R] {
  def actual: R
}

class ScenarioException[P, R](val scenario: Scenario[P, R], msg: String) extends Exception(msg) {

}

class ReasonInvalidException[P, R](s: Scenario[P, R])
  extends ScenarioException(s, s"Scenario defined at ${s.definedInSourceCodeAt} cannot be added because the reason given isn't valid")

class CannotAddScenarioException[P, R](s: Scenario[P, R], existing: Scenario[P, R], val actual: R)(implicit dp: DisplayProcessor) extends ScenarioException(
  s, s"Scenario defined at ${s.definedInSourceCodeAt} conflicts with ${existing.definedInSourceCodeAt}\n" +
    s"Scenario being added is ${dp.summary(s)}\n" +
    s"Scenario already existing is ${dp.summary(existing)}\n" +
    s"If it was added, would come to result ${dp.summary(actual)}") with HasActual[R] {
}


object AssertionInvalidException {
  def apply[P, R](s: Scenario[P, R], actualValue: R) = s.assertion match {
    case EqualsAssertion(expected) => new AssertionInvalidException[P, R](s, actualValue, s"The scenario defined at ${s.definedInSourceCodeAt} does not come to the result it is supposed to\n" +
      s"Expected result: ${expected}\n" +
      s"Actual result :  $actualValue")
  }
}


class AssertionInvalidException[P, R](s: Scenario[P, R], val actual: R, msg: String) extends ScenarioException(s, msg) with HasActual[R]


class CalculatorNotGivenException(scenarioDefinedAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at $scenarioDefinedAt has not been given a way of calculating a result")

class ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondByException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondBecauseException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSeconByRecursionException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondWhenException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt: DefinedInSourceCodeAt) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")
