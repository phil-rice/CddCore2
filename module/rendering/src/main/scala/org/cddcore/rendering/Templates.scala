package org.cddcore.rendering

import java.io._
import java.net.URI
import java.util
import java.util.Date

import com.github.mustachejava.{Mustache => JMustache, DefaultMustacheFactory}
import org.cddcore.engine.enginecomponents._
import org.cddcore.engine._
import org.cddcore.rendering.Renderer._
import org.cddcore.utilities.{DisplayProcessor, Maps}

import scala.io.Source

trait RenderConfiguration {
  def date: Date

  def urlBase: String

  def urlManipulations: UrlManipulations

}

object RenderConfiguration {

  implicit object DefaultRenderConfiguration extends RenderConfiguration {
    def date: Date = new Date

    val urlBase: String = "./target/cdd/"

    val urlManipulations: UrlManipulations = new FileUrlManipulations
  }

}

case class SimpleRenderConfiguration(urlBase: String, date: Date = new Date(), urlManipulations: UrlManipulations = new FileUrlManipulations) extends RenderConfiguration

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

  def idPath(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlManipulations.makeUrl(urlBase, idPath(ec))

  def makeFile(ec: EngineComponent[_, _], textForEngine: String) = urlManipulations.makeFile(url(ec), textForEngine)


}

trait TestObjectsForRendering {
  val displayProcessorModifiedForSituations = DisplayProcessor.defaultDisplayProcessor.withSummarize { case (dp, x) => s"Summary($x)" }
  protected val emptyEngine = new Engine[Int, String]("someEngineTitle") {}
  protected val engineWithUseCase = new Engine[Int, String]("someEngineTitle2") {
    useCase("someUseCase") {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2)
    }
    useCase("someOtherCase") {
      3 produces "three" when (_ == 3)
    }
    4 produces "four"
  }
  val List(useCase1: UseCase[Int, String], useCase2: UseCase[Int, String], scenario4: Scenario[Int, String]) = engineWithUseCase.builder.asUseCase.components.reverse
  val List(scenario1: Scenario[_, _], scenario2: Scenario[_, _]) = useCase1.components.reverse
  val List(scenario3: Scenario[_, _]) = useCase2.components.reverse


  protected val rc: RenderContext = RenderContext(new Date(), "urlBase", PathMap(emptyEngine, engineWithUseCase), new FileUrlManipulations())(displayProcessorModifiedForSituations)
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

  val linkKey = "link"
  val idKey = "id"
  val titleKey = "title"
  val linkUrlKey = "linkUrl"
  val iconUrlKey = "iconUrl"

  val definedAtKey = "definedAt"
  val selectedPostFixKey = "selected"

  val conclusionNodeKey = "conclusionNode"
  val decisionNodeKey = "decisionNode"
  val conditionKey = "condition"
  val conclusionKey = "conclusion"
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
  val engineWithTestsIcon = "images/engine.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png"
  val useCasesIcon = "images/usecase.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png"
  val scenarioIcon = "images/scenario.png" //http://imagizer.imageshack.us/a/img537/7868/P3Ucx2.png"
}

object Mustache {
  val mf = new DefaultMustacheFactory();

  def apply(name: String, template: String) = new Mustache(mf.compile(new StringReader(template), name))

  def apply(name: String) = new Mustache(mf.compile(name))


}

class Mustache(mustache: JMustache) {
  def apply(item: Any) = {
    val writer = new StringWriter()
    mustache.execute(writer, Templates.forMustache(item))
    writer.flush()
    writer.toString
  }
}

trait ExpectedForTemplates extends TestObjectsForRendering with KeysForRendering with Icons {
  protected val linkForScenario1 = Map(titleKey -> scenario1.title, linkUrlKey -> rc.url(scenario1), iconUrlKey -> scenarioIcon, definedAtKey -> scenario1.definedInSourceCodeAt)
  protected val linkForScenario2 = Map(titleKey -> scenario2.title, linkUrlKey -> rc.url(scenario2), iconUrlKey -> scenarioIcon, definedAtKey -> scenario2.definedInSourceCodeAt)
  protected val linkForScenario3 = Map(titleKey -> scenario3.title, linkUrlKey -> rc.url(scenario3), iconUrlKey -> scenarioIcon, definedAtKey -> scenario3.definedInSourceCodeAt)
  protected val linkForScenario4 = Map(titleKey -> scenario4.title, linkUrlKey -> rc.url(scenario4), iconUrlKey -> scenarioIcon, definedAtKey -> scenario4.definedInSourceCodeAt)
  protected val linkForUseCase1 = Map(titleKey -> useCase1.title, linkUrlKey -> rc.url(useCase1), iconUrlKey -> useCasesIcon, definedAtKey -> useCase1.definedInSourceCodeAt)
  protected val linkForUseCase2 = Map(titleKey -> useCase2.title, linkUrlKey -> rc.url(useCase2), iconUrlKey -> useCasesIcon, definedAtKey -> useCase2.definedInSourceCodeAt)
  protected val linkForEngine = Map(titleKey -> engineWithUseCase.title, linkUrlKey -> rc.url(engineWithUseCase), iconUrlKey -> engineWithTestsIcon, definedAtKey -> engineWithUseCase.definedInSourceCodeAt)

  protected val emptyUsecasesAndScenarios = Map(scenariosKey -> List(), useCasesKey -> List())

  protected val dataForScenario1 = Map(
    idKey -> rc.idPath(scenario1),
    typeKey -> scenarioTypeName,
    titleKey -> scenario1.title,
    linkKey -> linkForScenario1,
    situationKey -> "Summary(1)")


  protected val dataForScenario2 = Map(
    idKey -> rc.idPath(scenario2),
    typeKey -> scenarioTypeName,
    titleKey -> scenario2.title,
    linkKey -> linkForScenario2,
    situationKey -> "Summary(2)")


  protected val dataForScenario3 = Map(
    idKey -> rc.idPath(scenario3),
    typeKey -> scenarioTypeName,
    titleKey -> scenario3.title,
    linkKey -> linkForScenario3,
    situationKey -> "Summary(3)")


  protected val dataForScenario4 = Map(
    idKey -> rc.idPath(scenario4),
    typeKey -> scenarioTypeName,
    titleKey -> scenario4.title,
    linkKey -> linkForScenario4,
    situationKey -> "Summary(4)")


  protected val dataForUseCase1 = Map(
    idKey -> rc.idPath(useCase1),
    typeKey -> useCaseTypeName,
    titleKey -> "someUseCase",
    linkKey -> linkForUseCase1,
    scenariosIconsKey -> List(linkForScenario1, linkForScenario2))

  protected val dataForUseCase2 = Map(
    idKey -> rc.idPath(useCase2),
    typeKey -> useCaseTypeName,
    titleKey -> "someOtherCase",
    linkKey -> linkForUseCase2,
    scenariosIconsKey -> List(linkForScenario3))

  protected val dataForEngine = Map(
    idKey -> rc.idPath(engineWithUseCase),
    typeKey -> engineTypeName,
    titleKey -> engineWithUseCase.title,
    linkKey -> linkForEngine,
    scenariosIconsKey -> List(linkForScenario4))

  protected val pathForScenario1 = scenario1 :: useCase1 :: engineWithUseCase :: Nil
  protected val focusOnScenario1 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(
        dataForUseCase1 ++ Map(
          useCasesKey -> List(),
          scenariosKey -> List(dataForScenario1)
        )
      ))
  protected val pathForScenario2 = scenario2 :: useCase1 :: engineWithUseCase :: Nil
  protected val focusOnScenario2 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(
        dataForUseCase1 ++ Map(
          useCasesKey -> List(),
          scenariosKey -> List(dataForScenario2)
        )
      ))

  protected val pathForScenario3 = scenario3 :: useCase2 :: engineWithUseCase :: Nil
  protected val focusOnScenario3 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(
        dataForUseCase2 ++ Map(
          useCasesKey -> List(),
          scenariosKey -> List(dataForScenario3)
        )
      ))

  protected val pathForScenario4 = scenario4 :: useCase2 :: engineWithUseCase :: Nil
  protected val focusOnScenario4 =
    dataForEngine ++ Map(
      scenariosKey -> List(dataForScenario4),
      useCasesKey -> List())

  protected val pathForUseCase1 = useCase1 :: engineWithUseCase :: Nil
  protected val focusOnUseCase1FromUseCase1 =
    dataForUseCase1 ++ Map(
      useCasesKey -> List(),
      scenariosKey -> List(dataForScenario1, dataForScenario2)
    )

  protected val focusOnUseCase1 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(focusOnUseCase1FromUseCase1))

  protected val pathForUseCase2 = useCase2 :: engineWithUseCase :: Nil
  protected val focusOnUseCase2FromUseCase2 =
    dataForUseCase2 ++ Map(
      useCasesKey -> List(),
      scenariosKey -> List(dataForScenario3))

  protected val focusOnUseCase2 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(focusOnUseCase2FromUseCase2))

  protected val pathForEngine = engineWithUseCase :: Nil
  protected val focusOnEngine =
    dataForEngine ++ Map(
      scenariosKey -> List(scenario4),
      useCasesKey -> List(dataForUseCase1, dataForUseCase2))
  protected val focusOnEngineFromUseCase1 =
    dataForEngine ++ Map(
      scenariosKey -> List(),
      useCasesKey -> List(dataForUseCase1))

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

  object renderData extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, engineWithUseCase) produces dataForEngine by { case (rc, ec) =>
      findScenarioChildrenLinks(rc, ec) ++ Map(
        linkKey -> makeLink(rc, ec),
        idKey -> rc.idPath(ec),
        typeKey -> findTypeName(ec),
        titleKey -> ec.title)
    }
    (rc, useCase1) produces dataForUseCase1
    (rc, useCase2) produces dataForUseCase2

    (rc, scenario1) produces dataForScenario1 because { case (rc, s: Scenario[_, _]) =>
      Map(
        idKey -> rc.idPath(s),
        typeKey -> findTypeName(s),
        linkKey -> makeLink(rc, s),
        titleKey -> s.title,
        situationKey -> rc.displayProcessor(s.situation))
    }

    (rc, scenario2) produces dataForScenario2
    (rc, scenario3) produces dataForScenario3
    (rc, scenario4) produces dataForScenario4
  }


  def renderFocus(rc: RenderContext, ec: EngineComponent[_, _], path: Seq[EngineComponent[_, _]]): Map[String, _] = {

    val mapFromData = if (path.contains(ec))
      renderData(rc, ec) ++ Map(
        useCasesKey -> findUseCaseChildren(ec).map(uc =>
          if (path.contains(uc))
            renderFocus(rc, uc, path)
          else
            renderData(rc, uc) ++ emptyUsecasesAndScenarios ++ mapHoldingSelected(path, uc)),
        scenariosKey -> findScenarioChildren(ec).map(s => renderData(rc, s) ++ mapHoldingSelected(path, s))
      )
    else
      Map()

    mapHoldingSelected(path, ec) ++ mapFromData
  }

  def renderPath(rc: RenderContext, path: List[EngineComponent[_, _]]) =
    Map("title" -> path.head.title,
      "definedAt" -> path.head.definedInSourceCodeAt,
      "engine" -> Templates.renderFocus(rc, path.last, path))


  //  object RenderFocus extends Engine3[RenderContext, EngineComponent[_, _], Seq[EngineComponent[_, _]], Map[String, _]] {
  //    useCase("Path for engine") {
  //      (rc, engineWithUseCase, pathForEngine) produces focusOnEngine byRecursion { case (fn, (rc, e, path)) =>
  //        renderData(rc, e) ++ Map(
  //          useCasesKey -> findUseCaseChildren(e).filter(path.contains).map(fn(rc, _, path)),
  //          scenariosKey -> findScenarioChildren(e).filter(path.contains).map(renderData(rc, _))
  //        )
  //      }
  //      (rc, useCase1, pathForEngine) produces focusOnUseCase1
  //      (rc, useCase2, pathForEngine) produces focusOnUseCase2FromUseCase2
  //      (rc, scenario1, pathForEngine) produces dataForScenario1 ++ emptyUsecasesAndScenarios
  //      (rc, scenario2, pathForEngine) produces dataForScenario2 ++ emptyUsecasesAndScenarios
  //      (rc, scenario3, pathForEngine) produces dataForScenario3 ++ emptyUsecasesAndScenarios
  //      (rc, scenario4, pathForEngine) produces dataForScenario4 ++ emptyUsecasesAndScenarios
  //
  //    }
  //    useCase("Path for usecase1") {
  //      (rc, engineWithUseCase, pathForUseCase1) produces focusOnEngineFromUseCase1
  //      (rc, useCase1, pathForUseCase1) produces focusOnUseCase1FromUseCase1
  //      (rc, scenario1, pathForUseCase1) produces dataForScenario1 ++ emptyUsecasesAndScenarios
  //      (rc, scenario2, pathForUseCase1) produces dataForScenario2 ++ emptyUsecasesAndScenarios
  //      (rc, useCase2, pathForUseCase1) produces Map[String, Any]()
  //      (rc, scenario2, pathForUseCase1) produces Map[String, Any]()
  //      //      (rc, engineWithUseCase, pathForUseCase2) produces focusOnUseCase2
  //      //      (rc, engineWithUseCase, pathForScenario1) produces focusOnScenario1
  //      //      (rc, engineWithUseCase, pathForScenario2) produces focusOnScenario2
  //      //      (rc, engineWithUseCase, pathForScenario3) produces focusOnScenario3
  //      //      (rc, engineWithUseCase, pathForScenario4) produces focusOnScenario4
  //
  //      //      (rc, useCase1, pathForUseCase1) produces focusOnUseCase1FromUseCase1
  //      //      (rc, useCase2, pathForUseCase2) produces focusOnUseCase2FromUseCase2
  //      //      (rc, scenario1, pathForScenario1) produces dataForScenario1
  //      //      (rc, scenario2, pathForScenario2) produces dataForScenario2
  //      //      (rc, scenario3, pathForScenario3) produces dataForScenario3
  //      //      (rc, scenario4, pathForScenario4) produces dataForScenario4
  //
  //    }
  //  }


  def forMustache(s: Any): Any = s match {
    case m: Map[_, _] => m.foldLeft(new util.HashMap[String, Any]) {
      case (acc, (k: String, v)) => acc.put(k, forMustache(v))
        acc
    }
    case l: List[_] => l.foldLeft(new util.ArrayList[Any]) {
      (acc, v) => acc.add(forMustache(v))
        acc
    }
    case _ => s
  }

}

object DecisionTreeRendering extends KeysForRendering {

  def findSelected[P, R](engine: P => R, path: List[DecisionTree[P, R]], ec: EngineComponent[P, R]) = {
    val raw = mapHoldingSelected(path, ec)
    (path.headOption, ec) match {
      case (Some(dn: DecisionNode[P, R]), s: Scenario[P, R]) if (raw(selectedPostFixKey) == "" && dn.mainScenario.isDefinedAt(engine, s.situation)) =>
        Map(selectedPostFixKey -> "WouldBeTrue")
      case _ => raw
    }
  }

  def renderConclusionNode[P, R](engine: P => R, cn: ConclusionNode[P, R], path: List[DecisionTree[P, R]])(implicit dp: DisplayProcessor): Map[String, Any] = {
    findSelected(engine, path, cn) ++ Map(conclusionKey -> cn.mainScenario.toSummary(dp))
  }

  def renderDecisionNode[P, R](engine: P => R, dn: DecisionNode[P, R], path: List[DecisionTree[P, R]])(implicit dp: DisplayProcessor): Map[String, Any] = {
    findSelected(engine, path, dn) ++ Map(
      conditionKey -> dn.mainScenario.toSummary(dp),
      trueNodeKey -> render(engine, dn.trueNode, path),
      falseNodeKey -> render(engine, dn.falseNode, path))
  }

  def renderEngine[P, R](engine: Engine[P, R], ec: EngineComponent[P, R]): Map[String, Any] = {
    val path: List[DecisionTree[P, R]] = ec match {
      case s: Scenario[P, R] => engine.decisionTree.pathFor(engine, s)
      case _ => List()
    }
    render(engine, engine.decisionTree, path.reverse)
  }

  def render[P, R](engine: P => R, dt: DecisionTree[P, R], path: List[DecisionTree[P, R]]): Map[String, Any] = dt match {
    case cn: ConclusionNode[_, _] => Map(conclusionNodeKey -> renderConclusionNode(engine, cn, path), decisionNodeKey -> List())
    case dn: DecisionNode[_, _] => Map(conclusionNodeKey -> List(), decisionNodeKey -> renderDecisionNode(engine, dn, path))
  }
}
