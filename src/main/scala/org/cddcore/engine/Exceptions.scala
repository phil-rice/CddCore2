package org.cddcore.engine

class ReasonInvalidException(s: Scenario[_, _])
  extends Exception(s"Scenario defined at ${s.definedInSourceCodeAt} cannot be added because the reason given isn't actually true")

class CannotAddScenarioException(s: Scenario[_, _], existing: Scenario[_, _]) extends Exception(
  s"Scenario defined at ${s.definedInSourceCodeAt} conflicts with ${existing.definedInSourceCodeAt}\n" +
    s"Scenario being added is $s\n" +
    s"Scenario already existing is $existing"
)

class WrongResultProducedException(s: Scenario[_, _], actual: Any) extends Exception(s"The scenarion defined at ${s.definedInSourceCodeAt} does not come to the result it is supposed to\n" +
  s"Expected result: ${s.expected}\n" +
  s"Actual result :  $actual")
