package org.cddcore.rendering

import java.io._
import java.util.Date

import org.cddcore.engine._
import org.cddcore.enginecomponents.{EngineComponent, Scenario, UseCase}
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

  private val initialFiles = List("images/engine.png", "images/scenario.png", "images/usecase.png", "stylesheets/css.css")

  def populateInitialFiles(urlBase: String) = {
    initialFiles.foreach(f => copyFromClassPathToFile(f, new File(urlBase + "/" + f)))

  }
}


case class RenderContext(reportDate: Date, urlBase: String, pathMap: PathMap, urlManipulations: UrlManipulations)(implicit val displayProcessor: DisplayProcessor) {
  override def toString = getClass.getSimpleName()

  val inversePathMap = pathMap.inversePathMap

  def idPath(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlManipulations.makeUrl(urlBase, idPath(ec))

  def makeFile(ec: EngineComponent[_, _], textForEngine: String) = urlManipulations.makeFile(url(ec), textForEngine)
}


trait KeysForRendering {
  val mainEngineKey = "mainEngine"
  val decisionTreeKey = "decisionTree"

  val engineTypeName = "Engine"
  val useCaseTypeName = "UseCase"
  val scenarioTypeName = "Scenario"
  val situationKey = "situation"
  val expectedKey = "expected"

  val scenariosKey = "scenarios"
  val scenariosIconsKey = "scenarioIcons"
  val useCasesKey = "useCases"

  val typeKey = "type"
  val commentKey = "comment"
  val linkKey = "link"
  val idKey = "id"
  val titleKey = "title"
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
    (rc, engineWithUseCase) produces dataForEngine by { case (rc, ec) => renderRawData(rc, ec) }

    (rc, useCase1) produces dataForUseCase1 because { case (rc, uc: UseCase[_, _]) => renderRawData(rc, uc) ++ Map(commentKey -> uc.comment.getOrElse("")) }
    (rc, useCase2) produces dataForUseCase2

    (rc, scenario1) produces dataForScenario1 because { case (rc, s: Scenario[_, _]) =>
      Map(
        idKey -> rc.idPath(s),
        typeKey -> findTypeName(s),
        linkKey -> makeLink(rc, s),
        titleKey -> s.title,
        commentKey -> s.comment.getOrElse(""),
        situationKey -> rc.displayProcessor(s.situation))
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

object DecisionTreeRendering extends KeysForRendering {

  object DecisionTreeRenderData {
    def apply[P, R](engine: Engine[P, R], currentComponent: EngineComponent[P, R])(implicit dp: DisplayProcessor): DecisionTreeRenderData[P, R] = {
      val (path, scenario) = currentComponent match {
        case s: Scenario[P, R] => (engine.decisionTree.pathFor(engine, s).reverse, Some(s))
        case _ => (List(), None)
      }
      DecisionTreeRenderData(engine, scenario, path)
    }
  }

  case class DecisionTreeRenderData[P, R](engine: P => R, selectedScenario: Option[Scenario[P, R]], pathThroughDecisionTree: List[DecisionTree[P, R]])(implicit val dp: DisplayProcessor) {
    def findTrueFalse(dt: DecisionTree[P, R]): Map[String, Any] = Map(trueFalseKey -> (selectedScenario match {
      case Some(s) => dt.mainScenario.isDefinedAt(engine, s.situation).toString
      case _ => ""
    }))

    def selectedMap(dt: DecisionTree[P, R]): Map[String, Any] = mapHoldingSelected(pathThroughDecisionTree, dt)

    def selectedAndTrueFalseMap(dt: DecisionTree[P, R]): Map[String, Any] = findTrueFalse(dt) ++ selectedMap(dt)
  }

  def findSelected[P, R](rd: DecisionTreeRenderData[P, R], dt: DecisionTree[P, R]) = rd.findTrueFalse(dt) ++ rd.selectedMap(dt)

  def renderEngine[P, R](engine: Engine[P, R], ec: EngineComponent[P, R]): Map[String, Any] = {
    val rd = DecisionTreeRenderData(engine, ec)
    render(rd, engine.decisionTree)
  }

  def render[P, R](rd: DecisionTreeRenderData[P, R], dt: DecisionTree[P, R]): Map[String, Any] = dt match {
    case cn: ConclusionNode[_, _] => Map(conclusionNodeKey -> renderConclusionNode(rd, cn), decisionNodeKey -> List())
    case dn: DecisionNode[_, _] => Map(conclusionNodeKey -> List(), decisionNodeKey -> renderDecisionNode(rd, dn))
  }

  def renderConclusionNode[P, R](rd: DecisionTreeRenderData[P, R], cn: ConclusionNode[P, R]): Map[String, Any] = {
    rd.selectedAndTrueFalseMap(cn) ++ Map(
      conclusionKey -> cn.mainScenario.assertion.toSummary(rd.dp),
      reasonKey -> cn.mainScenario.reason.prettyDescription
    )
  }

  def renderDecisionNode[P, R](rd: DecisionTreeRenderData[P, R], dn: DecisionNode[P, R]): Map[String, Any] = {
    rd.selectedAndTrueFalseMap(dn) ++ Map(
      reasonKey -> dn.mainScenario.reason.prettyDescription,
      trueNodeKey -> render(rd, dn.trueNode),
      falseNodeKey -> render(rd, dn.falseNode))
  }


}
