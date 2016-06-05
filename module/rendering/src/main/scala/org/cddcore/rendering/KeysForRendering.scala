package org.cddcore.rendering

import org.cddcore.enginecomponents.EngineComponent


trait KeysForRendering {
  //  val mainEngineKey = "mainEngine"
  val decisionTreeKey = "decisionTree"
  val traceKey = "trace"

  val durationKey = "duration"
  val engineTypeName = "Engine"
  val useCaseTypeName = "UseCase"
  val scenarioTypeName = "Scenario"
  val situationKey = "situation"
  val expectedKey = "expected"
  val actualKey = "actual"

  val scenariosKey = "scenarios"
  val scenariosIconsKey = "scenarioIcons"
  val useCasesKey = "useCases"

  val typeKey = "type"
  val commentKey = "comment"
  val linkKey = "link"
  val idKey = "id"
  val summaryKey = "summary"
  val titleKey = "title"
  val referencesKey = "references"
  val linkUrlKey = "linkUrl"
  val iconUrlKey = "iconUrl"

  val definedAtKey = "definedAt"
  val selectedPostFixKey = "selected"
  val trueFalseKey = "trueFalseKey"

  val conclusionNodeKey = "conclusionNode"
  val decisionNodeKey = "decisionNode"
  val conditionKey = "condition"
  val conclusionKey = "conclusion"
  val reasonKey = "reason"
  val trueNodeKey = "trueNode"
  val falseNodeKey = "falseNode"

  def mapHoldingSelected(path: Seq[EngineComponent[_, _]], ec: EngineComponent[_, _]): Map[String, _] = ec match {
    case _ if path.isEmpty => Map(selectedPostFixKey -> "")
    case _ if path.head eq ec => Map(selectedPostFixKey -> "Selected")
    case _ if path.contains(ec) => Map(selectedPostFixKey -> "OnPath")
    case _ => Map(selectedPostFixKey -> "")
  }

}