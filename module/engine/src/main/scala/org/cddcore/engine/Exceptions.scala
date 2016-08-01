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
    s"Original scenario is ${originalScenario}\n", cause = cause)

object ConflictingScenariosException {
  def apply[P, R](s: Scenario[P, R], existing: Scenario[P, R], actual: R)(implicit dp: DisplayProcessor) = {
    val msg = s"Scenario defined at ${s.definedInSourceCodeAt} conflicts with ${existing.definedInSourceCodeAt}\n" +
      s"Scenario being added is ${s.definedInSourceCodeAt} ${dp.summary(s)}\n" +
      s"Scenario already existing is ${existing.definedInSourceCodeAt} ${dp.summary(existing)}\n" +
      s"If it was added, would come to result\n  ${dp.summary(actual)}"
    val (explaination, advice) = (s.reason.hasWhy, existing.reason.hasWhy) match {
      case (true, false) => (
        List(s"the existing scenario ${existing.definedInSourceCodeAt} doesn't have a reason"),
        List(s"A reason could be added to scenario ${existing.definedInSourceCodeAt} with a 'when' or a 'because'")
        )
      case (false, true) => (
        List(s"this scenario doesn't have a reason"),
        List(s"A reason could be added to scenario ${s.definedInSourceCodeAt} with a 'when' or a 'because'")
        )
      case (false, false) => (
        List("neither of these scenarios has a reason, and a reason is needed to differentiate them"),
        List("A reason could be added to either scenario with a 'when' or a 'because'")
        )
      case (true, true) => (
        List("both of these scenarios have a reason, but both reasons are true for both scenarios"),
        List("One or both reasons have to be 'improved', so that they differentiate between the two scenarios")
        )

    }
    new ConflictingScenariosException(s, existing, actual, msg, advice, List(
      "The scenario being added comes to the wrong conclusion in the engine being constructed",
      "It is 'conflicting' with an existing scenario.",
      "The 'logic' about that scenario is being applied to the one being added",
      "CDD cannot differentiate between the two because"
    ) ::: explaination)
  }
}

abstract class ScenarioExceptionWithAdvice[P, R](s: Scenario[P, R], msg: String, val advice: List[String]) extends ScenarioException(s, mainMessage = msg, msg = s"$msg\nAdvice\n${advice.mkString("\n")}") with HasAdvice

class ConflictingScenariosException[P, R](s: Scenario[P, R], val existing: Scenario[P, R], val actual: R, msg: String, advice: List[String], val explaination: List[String])(implicit dp: DisplayProcessor)
  extends ScenarioExceptionWithAdvice(s, msg = msg, advice = advice) with ConflictingScenarioException[P, R] with HasActual[R] with HasExplaination


class AddingWithRedundantReason[P, R](s: Scenario[P, R], val existing: Scenario[P, R], advice: List[String] = List(), val explaination: List[String])(implicit displayProcessor: DisplayProcessor) extends
  ScenarioExceptionWithAdvice(s, advice = advice,
    msg = s"The scenario defined at ${s.definedInSourceCodeAt} comes to the same conclusion as the scenario defined at ${existing.definedInSourceCodeAt} and both scenarios have a 'reason'\n" +
      s"This scenario is: ${displayProcessor.summary(s)} ${s.definedInSourceCodeAt}\n" +
      s"   reason is ${s.reason.prettyDescription}\n" +
      s"Existing scenario is: ${displayProcessor.summary(existing)} ${existing.definedInSourceCodeAt}\n" +
      s"   reason is ${s.reason.prettyDescription}\n") with ConflictingScenarioException[P, R] with HasExplaination


class EngineIsNotDefined extends Exception
