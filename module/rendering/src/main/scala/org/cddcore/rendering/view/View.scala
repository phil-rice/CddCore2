package org.cddcore.rendering.view

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.rendering.RenderContext

object View {
  val Class = "class"
  val Stack = "stack"
  val Message = "message"
  val Advice = "advice"
  val Reason = "reason"
  val Explaination = "explaination"
  val Actual = "actual"

  val Title = "title"
  val LinkUrl = "linkUrl"
  val IconUrl = "iconUrl"
  val DefinedAt = "definedAt"

  val Id = "id"
  val Type = "type"
  val References = "references"
  val Comment = "comment"
  val Error = "errors"
  val decisionTreeKey = "decisionTree"
  val traceKey = "trace"

  val durationKey = "duration"
  val EngineTypeName = "Engine"
  val UseCaseTypeName = "UseCase"
  val ScenarioTypeName = "Scenario"
  val situationKey = "situation"
  val expectedKey = "expected"
  val actualKey = "actual"

  val scenariosKey = "scenarios"
  val scenariosIconsKey = "scenarioIcons"
  val useCasesKey = "useCases"

  val linkKey = "link"
  val summaryKey = "summary"

  val selectedPostFixKey = "selected"
  val trueFalseKey = "trueFalseKey"

  val conclusionNodeKey = "conclusionNode"
  val decisionNodeKey = "decisionNode"
  val conditionKey = "condition"
  val conclusionKey = "conclusion"
  val reasonKey = "reason"
  val trueNodeKey = "trueNode"
  val falseNodeKey = "falseNode"

  def findTypeName(e: EngineComponent[_, _]) = e match {
    case e: Engine[_, _] => EngineTypeName
    case u: UseCase[_, _] => UseCaseTypeName
    case s: Scenario[_, _] => ScenarioTypeName
  }

}

class Views {
  val exceptionView = new ExceptionView
  val iconUrlFinder: IconUrlFinder = new IconUrlFinder
  val linkView = new LinkView(iconUrlFinder)
  val scenarioView = new ScenarioView(linkView, exceptionView)
}

trait View[T] {
  def apply(t: T)(implicit renderContext: RenderContext): Map[String, Object]
}
