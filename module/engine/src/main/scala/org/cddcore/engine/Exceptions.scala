package org.cddcore.engine

import org.cddcore.enginecomponents.Scenario
import org.cddcore.utilities.DisplayProcessor

class ValidationException(list: List[ValidationReport[_, _]]) extends Exception(s"Engine has validation issues:\n" + list.mkString("\n"))

class NoLastException(msg: String = "Last can only be used if the last item defined was a scenario") extends Exception(msg)

class MockValueNotFoundException[P](val p: P) extends Exception(s"Value[$p]")

class RecursiveScenariosWithoutMocksException(definedInSourceCodeAt: String, mocks: Iterable[_], scenarioAndValue: List[(Scenario[_, _], Any)])(implicit dp: DisplayProcessor) extends
  Exception(s"EngineDefined at $definedInSourceCodeAt\nThe following scenarios don't have a mock:\n" +
    scenarioAndValue.map { case (s, p) => s"Mock for situation[$p] needed by $s" }.mkString("\n") + "\nValid mocks are:\n" + mocks.mkString("\n"))