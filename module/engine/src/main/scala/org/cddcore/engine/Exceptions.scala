/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents._
import org.cddcore.utilities.DisplayProcessor

class ValidationException(list: List[ValidationReport[_, _]]) extends Exception(s"Engine has validation issues:\n" + list.mkString("\n"))

class NoLastException(msg: String = "Last can only be used if the last item defined was a scenario") extends Exception(msg)

class MockValueNotFoundException[P](val p: P) extends Exception(s"Value[$p]")

class RecursiveScenariosWithoutMocksException(definedInSourceCodeAt: String, mocks: Iterable[_], scenarioAndValue: List[(Scenario[_, _], Any)])(implicit dp: DisplayProcessor) extends
  Exception(s"EngineDefined at $definedInSourceCodeAt\nThe following scenarios don't have a mock:\n" +
    scenarioAndValue.map { case (s, p) => s"Mock for situation[$p] needed by $s" }.mkString("\n") + "\nValid mocks are:\n" + mocks.mkString("\n"))

class ScenarioCausesExceptionInOtherScenariosWhenClause[P, R](s: Scenario[P, R], val originalScenario: Scenario[P, R], cause: Exception)(implicit dp: DisplayProcessor) extends
  ScenarioException[P, R](s, s"The scenario defined at ${s.definedInSourceCodeAt} caused an exception when evaluating the condition of the scenario defined at ${originalScenario}\n" +
    s"This scenario is ${s}\n" +
    s"Original scenario is ${originalScenario}\n", cause)

object CannotAddScenarioException {
  def apply[P, R](s: Scenario[P, R], existing: Scenario[P, R], actual: R)(implicit dp: DisplayProcessor) = {
    val msg = s"Scenario defined at ${s.definedInSourceCodeAt} conflicts with ${existing.definedInSourceCodeAt}\n" +
      s"Scenario being added is ${dp.summary(s)}\n" +
      s"Scenario already existing is ${dp.summary(existing)}\n" +
      s"If it was added, would come to result\n  ${dp.summary(actual)}"
    val adviceSuffix = "scenario with a 'when' or a 'because'"
    val advice = (s.reason.hasWhy, existing.reason.hasWhy) match {
      case (true, false) => "Neither of these scenarios has a reason. A reason could be added to either "
      case (true, true) => s"The reason it currently comes to that conclusion is\n  ${existing.reason.prettyDescription}\nThe two reasons are applicable to both of the scenarios, so at least one of them will have to be refined"
      case (false, false) => s"Neither of these scenarios has a reason. A reason could be added to either $adviceSuffix"
      case (false, true) => s"The reason it currently comes to that conclusion is\n  ${existing.reason.prettyDescription}\nA reason could be added to this $adviceSuffix "
    }
    new CannotAddScenarioException(s, existing, actual, msg, advice)
  }
}

class CannotAddScenarioException[P, R](s: Scenario[P, R], val existing: Scenario[P, R], val actual: R, msg: String, val advice: String)(implicit dp: DisplayProcessor) extends ScenarioException(
  s, msg) with HasActual[R] with ConflictingScenarioException[P, R] with HasAdvice


class AddingWithRedundantReason[P, R](s: Scenario[P, R], val existing: Scenario[P, R], val advice: String = "")(implicit displayProcessor: DisplayProcessor) extends
  ScenarioException(s, s"The scenario defined at ${s.definedInSourceCodeAt} comes to the same conclusion as the scenario defined at ${existing.definedInSourceCodeAt} and both scenarios have a 'reason'\n" +
    s"This scenario is: ${displayProcessor.summary(s)} ${s.definedInSourceCodeAt}\n" +
    s"   reason is ${s.reason.prettyDescription}\n" +
    s"Existing scenario is: ${displayProcessor.summary(existing)} ${existing.definedInSourceCodeAt}\n" +
    s"   reason is ${s.reason.prettyDescription}\n") with ConflictingScenarioException[P, R] with HasAdvice
