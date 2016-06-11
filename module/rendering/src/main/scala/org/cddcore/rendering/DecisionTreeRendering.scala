package org.cddcore.rendering

import org.cddcore.engine._
import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.DisplayProcessor

object DecisionTreeRenderData {
  def fromEngineComponent[P, R](engine: Engine[P, R], ec: EngineComponent[P, R])(implicit dp: DisplayProcessor): DecisionTreeRenderData[P, R] = ec match {
    case s: Scenario[P, R] => fromSituation(engine, Some(s.situation))
    case _ => fromSituation(engine, None)
  }

  def fromSituation[P, R](engine: AbstractEngine[P, R], situation: Option[P])(implicit dp: DisplayProcessor): DecisionTreeRenderData[P, R] = {
    val (path, s) = situation match {
      case Some(s) => (engine.decisionTree.pathFor(engine.evaluate, s).reverse, Some(s))
      case _ => (List(), None)
    }
    val result = DecisionTreeRenderData(engine.evaluate, s, path)
    result
  }
}

case class DecisionTreeRenderData[P, R](engine: P => R, selectedSituation: Option[P], pathThroughDecisionTree: List[DecisionTree[P, R]])(implicit val dp: DisplayProcessor) extends KeysForRendering {
  def findTrueFalse(dt: DecisionTree[P, R]): Map[String, Any] = Map(trueFalseKey -> (selectedSituation match {
    case Some(s) => try {
      dt.mainScenario.isDefinedAt(engine, s).toString
    } catch {
      case e: Exception => ""
    }
    case _ => ""
  }))

  def selectedMap(dt: DecisionTree[P, R]): Map[String, Any] = mapHoldingSelected(pathThroughDecisionTree, dt)

  def selectedAndTrueFalseMap(dt: DecisionTree[P, R]): Map[String, Any] = findTrueFalse(dt) ++ selectedMap(dt)
}

object DecisionTreeRendering extends KeysForRendering {

  def findSelected[P, R](rd: DecisionTreeRenderData[P, R], dt: DecisionTree[P, R]) = rd.findTrueFalse(dt) ++ rd.selectedMap(dt)

  def renderEngine[P, R](engine: Engine[P, R], ec: EngineComponent[P, R])(implicit displayProcessor: DisplayProcessor): Map[String, Any] = {
    val rd = DecisionTreeRenderData.fromEngineComponent(engine, ec)
    render(rd, engine.decisionTree)
  }

  def render[P, R](rd: DecisionTreeRenderData[P, R], dt: DecisionTree[P, R])(implicit displayProcessor: DisplayProcessor): Map[String, Any] = dt match {
    case cn: ConclusionNode[_, _] => Map(conclusionNodeKey -> renderConclusionNode(rd, cn), decisionNodeKey -> List())
    case dn: DecisionNode[_, _] => Map(conclusionNodeKey -> List(), decisionNodeKey -> renderDecisionNode(rd, dn))
  }

  def renderConclusionNode[P, R](rd: DecisionTreeRenderData[P, R], cn: ConclusionNode[P, R])(implicit displayProcessor: DisplayProcessor): Map[String, Any] = {
    rd.selectedAndTrueFalseMap(cn) ++ Map(
      conclusionKey -> cn.mainScenario.assertion.summary(rd.dp),
      reasonKey -> cn.mainScenario.reason.prettyDescription
    )
  }

  def renderDecisionNode[P, R](rd: DecisionTreeRenderData[P, R], dn: DecisionNode[P, R])(implicit displayProcessor: DisplayProcessor): Map[String, Any] = {
    rd.selectedAndTrueFalseMap(dn) ++ Map(
      reasonKey -> dn.mainScenario.reason.prettyDescription,
      trueNodeKey -> render(rd, dn.trueNode),
      falseNodeKey -> render(rd, dn.falseNode))
  }
}
