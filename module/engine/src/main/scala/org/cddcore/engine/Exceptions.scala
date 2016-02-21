package org.cddcore.engine

import org.cddcore.engine.enginecomponents.Scenario

class ValidationException(list: List[ValidationReport[_, _]]) extends Exception(s"Engine has validation issues:\n" + list.mkString("\n"))

class NoLastException(msg: String = "Last can only be used if the last item defined was a scenario") extends Exception(msg)

class MockValueNotFoundException extends Exception

class RecursiveScenariosWithoutMocksException(mocks: Iterable[_], scenarios: List[Scenario[_, _]]) extends
  Exception(s"The following scenarios don't have a mock:\n" + scenarios.mkString("\n") + "\nValid mocks are:\n" + mocks.mkString("\n"))