/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import java.io._
import java.util.Date

import org.cddcore.engine._
import org.cddcore.enginecomponents._
import org.cddcore.utilities.DisplayProcessor

case class RenderConfiguration(date: Date = new Date, urlBase: String = "./target/cdd", urlManipulations: UrlManipulations = new FileUrlManipulations)

object RenderConfiguration {

  implicit val defaultRenderConfiguration = RenderConfiguration()

}


trait UrlManipulations {

  def makeUrl(urlBase: String, idPathResult: String): String

  def makeFile(url: String, text: String)

  def populateInitialFiles(urlBase: String)
}

class WebsiteUrlManipulators extends UrlManipulations {

  def makeUrl(urlBase: String, idPathResult: String) = urlBase + idPathResult

  def makeFile(url: String, text: String) = ???

  def populateInitialFiles(urlBase: String) = ???

}

class FileUrlManipulations extends UrlManipulations {

  private def transfer(in: InputStream, out: OutputStream) {
    val BufferSize = 8192
    val buffer = new Array[Byte](BufferSize)
    def read() {
      val byteCount = in.read(buffer)
      if (byteCount >= 0) {
        out.write(buffer, 0, byteCount)
        read()
      }
    }
    read()
  }

  def copyFromClassPathToFile(resourceId: String, file: File): Unit = {
    def useClosable[S <: Closeable, X](makeS: => S)(useS: S => X) = {
      val s = makeS
      try {
        useS(s)
      } finally {
        s.close()
      }
    }
    file.getParentFile.mkdirs()
    file.createNewFile()
    useClosable(getClass.getClassLoader.getResourceAsStream(resourceId))(
      inputStream => useClosable(new FileOutputStream(file))(outputStream =>
        transfer(inputStream, outputStream)))
  }

  def makeUrl(urlBase: String, idPathResult: String) = new File(urlBase + "/" + idPathResult + ".html").getCanonicalFile.toString

  def makeFile(url: String, text: String): Unit = {
    val file = new File(url)
    //    println("Make file: " + file.getAbsoluteFile)
    file.getParentFile.mkdirs()
    val writer = new FileWriter(file)
    try {
      writer.write(text)
    } finally (writer.close)
  }

  private val initialFiles = List("images/engine.png", "images/scenario.png", "images/usecase.png", "images/document.png", "images/cdd.png", "stylesheets/css.css")

  def populateInitialFiles(urlBase: String) = initialFiles.foreach(f => copyFromClassPathToFile(f, new File(urlBase + "/" + f)))
}


case class RenderContext(reportDate: Date, urlBase: String, pathMap: PathMap, urlManipulations: UrlManipulations)(implicit val displayProcessor: DisplayProcessor) {
  override def toString = getClass.getSimpleName()

  val inversePathMap = pathMap.inversePathMap

  def idPath(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlManipulations.makeUrl(urlBase, idPath(ec))

  def makeFile(ec: EngineComponent[_, _], textForEngine: String) = urlManipulations.makeFile(url(ec), textForEngine)
}


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

trait ReferenceMapMakers {
  def documentToMap(d: Document) = Map("name" -> d.name, "ref" -> d.ref)

  def referenceToMap(rc: RenderContext)(r: Reference) = Map("document" -> documentToMap(r.document), "internalRef" -> r.internalRef, "imgSrc" -> (rc.urlBase + "images/document.png"))

}

trait Icons {
  val engineWithTestsIcon = "../images/engine.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png"
  val useCasesIcon = "../images/usecase.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png"
  val scenarioIcon = "../images/scenario.png" //http://imagizer.imageshack.us/a/img537/7868/P3Ucx2.png"
}


object Templates extends TestObjectsForRendering with Icons with KeysForRendering with ExpectedForTemplates {

  val findIconUrl = new Engine[EngineComponent[_, _], String] {
    emptyEngine produces engineWithTestsIcon because { case e: Engine[_, _] => engineWithTestsIcon }
    engineWithUseCase produces engineWithTestsIcon
    useCase1 produces useCasesIcon because { case _: UseCase[_, _] => useCasesIcon }
    scenario1 produces scenarioIcon because { case _: Scenario[_, _] => scenarioIcon }
    scenario2 produces scenarioIcon
    scenario3 produces scenarioIcon
  }

  val makeLink = new Engine2[RenderContext, EngineComponent[_, _], Map[String, _]]("Produces the maps for a link to a component") {
    (rc, engineWithUseCase) produces linkForEngine by {
      case (rc, ec) => Map(titleKey -> ec.title, linkUrlKey -> rc.url(ec), iconUrlKey -> findIconUrl(ec), definedAtKey -> ec.definedInSourceCodeAt)
    }
    (rc, useCase1) produces linkForUseCase1
    (rc, useCase2) produces linkForUseCase2
    (rc, scenario2) produces linkForScenario2
    (rc, scenario3) produces linkForScenario3
  }

  //  val references = new Engine[EngineComponent[_, _], Seq[Map[String, Any]]]("produces the references map for an engine component") {
  //    scenario2 produces List(Map("name" -> "name2", "ref" -> "someHRef1")) because { case s: Scenario[_, _] => s.references.map(referenceToMap) }
  //    scenario1 produces List()
  //    useCase1 produces List(referenceToMap(Reference(d2))) because { case uc: UseCase[_, _] => uc.references.map(referenceToMap) }
  //    engineWithUseCase produces List(referenceToMap(Reference(d2, "engineRef"))) because { case e: Engine[_, _] => e.references.map(referenceToMap) }
  //
  //  }

  implicit val displayProcessorRemovingLinks = DisplayProcessor.defaultDisplayProcessor.withSummarize { case (dp, ("link", s)) => "link -> ?" }

  private def ucAndS(uc: List[EngineComponent[_, _]], s: List[EngineComponent[_, _]]) = Map(useCasesKey -> uc, scenariosKey -> s)

  private def partition(ecs: List[EngineComponent[_, _]]) =
    ucAndS(ecs.filter(_.isInstanceOf[UseCase[_, _]]), ecs.filter(_.isInstanceOf[Scenario[_, _]]))

  val findChildren = new Engine[EngineComponent[_, _], List[EngineComponent[_, _]]] {
    engineWithUseCase produces List(useCase1, useCase2, scenario4) because { case e: Engine[_, _] => e.asUseCase.components.reverse }
    emptyEngine produces List()
    useCase1 produces List(scenario1, scenario2) because { case uc: UseCase[_, _] => uc.components.reverse }
    useCase2 produces List(scenario3)
    scenario1 produces List() because { case s: Scenario[_, _] => List() }
    scenario2 produces List()
    scenario3 produces List()
  }

  val findScenarioChildren = new Engine[EngineComponent[_, _], List[EngineComponent[_, _]]] {
    engineWithUseCase produces List(scenario4) because { case e => findChildren(e).collect { case s: Scenario[_, _] => s } }
    emptyEngine produces List()
    useCase1 produces List(scenario1, scenario2)
    useCase2 produces List(scenario3)
    scenario1 produces List()
    scenario2 produces List()
    scenario3 produces List()
  }

  val findUseCaseChildren = new Engine[EngineComponent[_, _], List[EngineComponent[_, _]]] {
    engineWithUseCase produces List(useCase1, useCase2) because { case e => findChildren(e).collect { case uc: UseCase[_, _] => uc } }
    emptyEngine produces List()
    useCase1 produces List()
    useCase2 produces List()
    scenario1 produces List()
    scenario2 produces List()
    scenario3 produces List()
  }

  val findTypeName = new Engine[EngineComponent[_, _], String] {
    engineWithUseCase produces engineTypeName because {
      case e: Engine[_, _] => engineTypeName
    }
    useCase1 produces useCaseTypeName because {
      case u: UseCase[_, _] => useCaseTypeName
    }
    scenario1 produces scenarioTypeName because {
      case s: Scenario[_, _] => scenarioTypeName
    }
  }

  object findScenarioChildrenLinks extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, engineWithUseCase) produces Map(scenariosIconsKey -> List(linkForScenario4)) by { case (rc, ec) => Map(scenariosIconsKey -> findScenarioChildren(ec).map(makeLink(rc, _))) }
    (rc, useCase1) produces Map(scenariosIconsKey -> List(linkForScenario1, linkForScenario2))
    (rc, useCase2) produces Map(scenariosIconsKey -> List(linkForScenario3))
    (rc, scenario1) produces Map(scenariosIconsKey -> List())
  }

  def renderRawData[P, R](rc: RenderContext, ec: EngineComponent[P, R]) = findScenarioChildrenLinks(rc, ec) ++ Map(
    linkKey -> makeLink(rc, ec),
    idKey -> rc.idPath(ec),
    typeKey -> findTypeName(ec),
    titleKey -> ec.title)

  object renderData extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, engineWithUseCase) produces dataForEngine because { case (rc, e: Engine[_, _]) => renderRawData(rc, e) ++ Map(referencesKey -> e.references.map(referenceToMap(rc))) }

    (rc, useCase1) produces dataForUseCase1 because { case (rc, uc: UseCase[_, _]) => renderRawData(rc, uc) ++ Map(commentKey -> uc.comment.getOrElse(""), referencesKey -> uc.references.map(referenceToMap(rc))) }
    (rc, useCase2) produces dataForUseCase2

    (rc, scenario1) produces dataForScenario1 because { case (rc, s: Scenario[_, _]) =>
      Map(
        idKey -> rc.idPath(s),
        typeKey -> findTypeName(s),
        linkKey -> makeLink(rc, s),
        titleKey -> s.title,
        commentKey -> s.comment.getOrElse(""),
        situationKey -> rc.displayProcessor(s.situation),
        referencesKey -> s.references.map(referenceToMap(rc)))
    }

    (rc, scenario2) produces dataForScenario2
    (rc, scenario3) produces dataForScenario3
    (rc, scenario4) produces dataForScenario4
  }


  def renderFocus(rc: RenderContext, ec: EngineComponent[_, _], path: Seq[EngineComponent[_, _]]): Map[String, _] = {
    val scenarios = if (path.contains((ec))) findScenarioChildren(ec) else List()

    val scenariosMap = Map(scenariosKey -> scenarios.map(s => renderData(rc, s) ++ mapHoldingSelected(path, s)))
    renderData(rc, ec) ++
      Map(useCasesKey -> findUseCaseChildren(ec).map(renderFocus(rc, _, path))) ++
      scenariosMap ++
      mapHoldingSelected(path, ec)
  }

  def renderPath(rc: RenderContext, path: List[EngineComponent[_, _]]) =
    Map("title" -> path.head.title,
      "definedAt" -> path.head.definedInSourceCodeAt,
      "engine" -> Templates.renderFocus(rc, path.last, path))
}

object TraceRendering extends ExpectedForTemplates {

  object renderTrace extends Engine2[RenderContext, Trace, Map[String, _]] {
    private def dtrd[P, R](et: TraceEngine[P, R]) = {
      val renderData = DecisionTreeRenderData.fromSituation(et.engine, Some(et.params))(rc.displayProcessor)
      DecisionTreeRendering.render(renderData, et.engine.decisionTree)(rc.displayProcessor)
    }

    (rc, trace1) produces Map(
      engineTypeName ->(titleKey -> trace1.engine.title, linkUrlKey -> rc.url(trace1.engine)),
      situationKey -> trace1.params,
      actualKey -> trace1.result,
      durationKey -> trace1.duration,
      decisionTreeKey -> DecisionTreeRendering.render(
        DecisionTreeRenderData.fromSituation(engineWithUseCase, Some(trace1.params))(rc.displayProcessor),
        engineWithUseCase.decisionTree)(rc.displayProcessor)) byRecursion {
      case (engine, (rc, et: TraceEngine[_, _])) =>
        Map(
          engineTypeName -> Map(titleKey -> et.engine.title, linkUrlKey -> rc.url(et.engine)),
          situationKey -> et.params,
          actualKey -> et.result,
          durationKey -> et.duration,
          decisionTreeKey -> dtrd(et),
          traceKey -> et.children.map(engine(rc, _)))
    }

  }

}

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
    case Some(s) => dt.mainScenario.isDefinedAt(engine, s).toString
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
      conclusionKey -> cn.mainScenario.assertion.toSummary(rd.dp),
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
