package org.cddcore.engine

import org.cddcore.engine.enginecomponents.Scenario
import org.cddcore.utilities.DisplayProcessor

class ValidationException(list: List[ValidationReport[_, _]]) extends Exception(s"Engine has validation issues:\n" + list.mkString("\n"))

class NoLastException(msg: String = "Last can only be used if the last item defined was a scenario") extends Exception(msg)

class MockValueNotFoundException(val p: Any) extends Exception

class RecursiveScenariosWithoutMocksException(mocks: Iterable[_], scenarioAndValue: List[(Scenario[_, _], Any)])(implicit dp: DisplayProcessor) extends
  Exception(s"The following scenarios don't have a mock:\n" +
    scenarioAndValue.map { case (s, p) => s"Mock for situation[$p] needed by $s" }.mkString("\n") + "\nValid mocks are:\n" + mocks.mkString("\n"))