/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
