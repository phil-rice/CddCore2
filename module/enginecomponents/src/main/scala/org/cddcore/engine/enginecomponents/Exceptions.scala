package org.cddcore.engine.enginecomponents

class ReasonInvalidException(s: Scenario[_, _])
  extends Exception(s"Scenario defined at ${s.definedInSourceCodeAt} cannot be added because the reason given isn't actually true")

class CannotAddScenarioException(s: Scenario[_, _], existing: Scenario[_, _], actual: Any) extends Exception(
  s"Scenario defined at ${s.definedInSourceCodeAt} conflicts with ${existing.definedInSourceCodeAt}\n" +
    s"Scenario being added is $s\n" +
    s"Scenario already existing is $existing\n" +
    s"If it was added, would come to result $actual"
)

object AssertionInvalidException {
  def apply(s: Scenario[_, _], actualValue: Any) = s.assertion match {
    case EqualsAssertion(expected) => new AssertionInvalidException(s, actualValue, s"The scenario defined at ${s.definedInSourceCodeAt} does not come to the result it is supposed to\n" +
      s"Expected result: ${expected}\n" +
      s"Actual result :  $actualValue")
  }
}


class AssertionInvalidException(s: Scenario[_, _], actual: Any, msg: String) extends Exception(msg)


class CalculatorNotGivenException(scenarioDefinedAt: String) extends Exception(s"Scenario defined at $scenarioDefinedAt has not been given a way of calculating a result")

class ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondByException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondBecauseException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")
class ScenarioCannotHaveSeconByRecursionException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveSecondWhenException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")

class ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt: String) extends Exception(s"Scenario defined at ${definedInSourceCodeAt}")