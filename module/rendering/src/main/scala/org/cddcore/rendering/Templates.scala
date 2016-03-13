package org.cddcore.rendering

import java.io.{FileWriter, File, StringWriter, StringReader}
import java.net.URI
import java.util
import java.util.Date

import com.github.mustachejava.{Mustache => JMustache, DefaultMustacheFactory}
import org.cddcore.engine.enginecomponents._
import org.cddcore.engine.{Engine3, Engine, Engine2}
import org.cddcore.rendering.Renderer._
import org.cddcore.utilities.{DisplayProcessor, Maps}

import scala.io.Source

trait RenderConfiguration {
  def date: Date

  def urlBase: String

}

object RenderConfiguration {

  implicit object DefaultRenderConfiguration extends RenderConfiguration {
    def date: Date = new Date

    def urlBase: String = "./target/cdd/"
  }

}

case class SimpleRenderConfiguration(urlBase: String, date: Date = new Date()) extends RenderConfiguration

case class RenderContext(reportDate: Date, urlBase: String, pathMap: PathMap)(implicit val displayProcessor: DisplayProcessor) {
  override def toString = getClass.getSimpleName()

  def idPath(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlBase + "/" + ec.getClass.getSimpleName + "/" + idPath(ec) +".html"

  def makeFile(ec: EngineComponent[_, _], textForEngine: String) = {
    val file = new File(url(ec))
    println("Making File: " + file)
    file.getParentFile.mkdirs()
    val writer = new FileWriter(file)
    try {
      writer.write(textForEngine)
    } finally (writer.close)
  }

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


  protected val rc: RenderContext = RenderContext(new Date(), "urlBase", PathMap(emptyEngine, engineWithUseCase))(displayProcessorModifiedForSituations)
}

trait KeysForRendering {
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

}

trait Icons {
  val engineWithTestsIcon = "http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png"
  val foldingEngineIcon = "http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png"
  val useCasesIcon = "http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png"
  val scenarioIcon = "http://imagizer.imageshack.us/a/img537/7868/P3Ucx2.png"
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
  protected val linkForScenario1 = Map(titleKey -> scenario1.title, linkUrlKey -> rc.url(scenario1), iconUrlKey -> scenarioIcon)
  protected val linkForScenario2 = Map(titleKey -> scenario2.title, linkUrlKey -> rc.url(scenario2), iconUrlKey -> scenarioIcon)
  protected val linkForScenario3 = Map(titleKey -> scenario3.title, linkUrlKey -> rc.url(scenario3), iconUrlKey -> scenarioIcon)
  protected val linkForScenario4 = Map(titleKey -> scenario4.title, linkUrlKey -> rc.url(scenario4), iconUrlKey -> scenarioIcon)
  protected val linkForUseCase1 = Map(titleKey -> useCase1.title, linkUrlKey -> rc.url(useCase1), iconUrlKey -> useCasesIcon)
  protected val linkForUseCase2 = Map(titleKey -> useCase2.title, linkUrlKey -> rc.url(useCase2), iconUrlKey -> useCasesIcon)
  protected val linkForEngine = Map(titleKey -> engineWithUseCase.title, linkUrlKey -> rc.url(engineWithUseCase), iconUrlKey -> engineWithTestsIcon)

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
    (rc, emptyEngine) produces Map(titleKey -> "someEngineTitle", linkUrlKey -> rc.url(emptyEngine), iconUrlKey -> engineWithTestsIcon) by {
      case (rc, ec) => Map(titleKey -> ec.title, linkUrlKey -> rc.url(ec), iconUrlKey -> findIconUrl(ec))
    }
    (rc, engineWithUseCase) produces linkForEngine
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
    if (path.contains(ec))
      renderData(rc, ec) ++ Map(
        useCasesKey -> findUseCaseChildren(ec).map(uc =>
          if (path.contains(uc))
            renderFocus(rc, uc, path)
          else
            renderData(rc, uc) ++ emptyUsecasesAndScenarios),
        scenariosKey -> findScenarioChildren(ec).map(renderData(rc, _))
      )
    else
      Map()
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
