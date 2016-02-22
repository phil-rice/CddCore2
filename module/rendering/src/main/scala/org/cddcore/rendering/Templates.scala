package org.cddcore.rendering

import java.util.Date

import mustache.Mustache
import org.cddcore.engine.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.engine.{Engine, Engine2}



case class RenderContext(reportDate: Date, urlBase: String, pathMap: PathMap) {
  override def toString = getClass.getSimpleName()

  def path(ec: EngineComponent[_, _]) = pathMap(ec)

  def url(ec: EngineComponent[_, _]) = urlBase + "/" + ec.getClass.getSimpleName + "/" + path(ec)

}

trait TestObjectsForRendering {
  protected val emptyEngine = new Engine[Int, String]("someEngineTitle") {}
  protected val engineWithUseCase = new Engine[Int, String]("someEngineTitle2") {
    useCase("someUseCase") {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2)
      3 produces "three"
    }
  }
  val List(useCase1: UseCase[Int, String]) = engineWithUseCase.builder.asUseCase.components
  val List(scenario1: Scenario[_, _], scenario2: Scenario[_, _], scenario3: Scenario[_, _]) = useCase1.components.reverse


  protected val rc: RenderContext = RenderContext(new Date(), "urlBase", PathMap(emptyEngine, engineWithUseCase))
}

trait KeysForRendering {
  val engineTypeName = "Engine"
  val useCaseTypeName = "UseCase"
  val scenarioTypeName = "Scenario"
  val situationKey = "situation"
  val expectedKey = "expected"

  val childrenKey = "children"

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

trait MustacheTemplates {
  val linkTemplate = new Mustache("{{#link}}<a href='{{linkUrl}}' alt='{{title}}'>{{title}}<img src='{{iconUrl}}' /></a>{{/link}}")
  val scenarioFullTemplate = new Mustache(
    """<div id='{{id}}' class='scenario'><div class='scenarioTitle'>{{title}} {{>link}}</div><!-- scenarioTitle -->
      |   <table class='scenarioTable'">{{#link}}{{>link}}{{/link}}
      |      <tr><td>Situation</td><td>{{situation}}</td></tr>
      |      <tr><td>Expected</td><td>{{expected}}</td></tr>
      |   </table>
      |</div><!-- scenario -->""".stripMargin)

  val useCaseFullTemplate = new Mustache(
    """<div id='{{id}}' class='usecase'>
      |    <div class='usecaseTitle'> {{title}} {{>link}} </div><!-- usecaseTitle -->
      |    {{#children}}{{>component}}{{/children}}
      |</div><!-- usecase' --> """.stripMargin)

  val engineFullTemplate = new Mustache(
    """<div id='{{id}}' class='engine'>{{
      |    <div class='usecaseTitle'>{{title}} {{>link}} </div><!-- usecaseTitle -->
      |    {{#children}}{{>component}}{{/children}}
      |</div><!-- usecase' --> """.stripMargin)

  val childrenTemplate = new Mustache(
    """{{#engine}}{{>engine}}{{/engine}}
      |{{#usecase}}{{>usecase}}{{/usecase}}
      |{{#scenario}}{{>scenario}}{{/scenario}}""".stripMargin)
}

trait ExpectedForTemplates extends TestObjectsForRendering with KeysForRendering with Icons {
  protected val expectedForScenario1Link = Map(titleKey -> scenario1.title, linkUrlKey -> rc.url(scenario1), iconUrlKey -> scenarioIcon)
  protected val expectedForScenario2Link = Map(titleKey -> scenario2.title, linkUrlKey -> rc.url(scenario2), iconUrlKey -> scenarioIcon)
  protected val expectedForScenario3Link =  Map(titleKey -> scenario3.title, linkUrlKey -> rc.url(scenario3), iconUrlKey -> scenarioIcon)
  protected val expectedForUseCase1Link = Map(titleKey -> useCase1.title, linkUrlKey -> rc.url(useCase1), iconUrlKey -> useCasesIcon)
  protected val expectedForEngineWithUseCaseLink =  Map(titleKey -> engineWithUseCase.title, linkUrlKey -> rc.url(engineWithUseCase), iconUrlKey -> engineWithTestsIcon)


  protected val expectedForScenario1 = Map(
    idKey -> rc.path(scenario1),
    typeKey -> scenarioTypeName,
    titleKey -> scenario1.title,
    linkKey -> expectedForScenario1Link,
    childrenKey -> List())

  protected val expectedForScenario2 = Map(
    idKey -> rc.path(scenario2),
    typeKey -> scenarioTypeName,
    titleKey -> scenario2.title,
    linkKey -> expectedForScenario2Link,
    childrenKey -> List())

  protected val expectedForScenario3 = Map(
    idKey -> rc.path(scenario3),
    typeKey -> scenarioTypeName,
    titleKey -> scenario3.title,
    linkKey -> expectedForScenario3Link,
    childrenKey -> List())

  protected val expectedForUseCase1 = Map(
    idKey -> rc.path(useCase1),
    typeKey -> useCaseTypeName,
    titleKey -> "someUseCase",
    linkKey -> expectedForUseCase1Link,
    childrenKey -> List(Map(scenarioTypeName -> expectedForScenario1), Map(scenarioTypeName -> expectedForScenario2), Map(scenarioTypeName -> expectedForScenario3)))

  protected val expectedForEngineWithUseCase = Map(
    idKey -> rc.path(engineWithUseCase),
    typeKey -> engineTypeName,
    titleKey -> "someEngineTitle2",
    linkKey -> expectedForEngineWithUseCaseLink,
    childrenKey -> List(expectedForUseCase1))

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

  val makeLink = new Engine2[RenderContext, EngineComponent[_, _], Map[String, Any]]("Produces the maps for a link to a component") {
    (rc, emptyEngine) produces Map(linkKey -> Map(titleKey -> "someEngineTitle", linkUrlKey -> rc.url(emptyEngine), iconUrlKey -> engineWithTestsIcon)) by {
      case (rc, ec) => Map(linkKey -> Map(titleKey -> ec.title, linkUrlKey -> rc.url(ec), iconUrlKey -> findIconUrl(ec)))
    }
    (rc, engineWithUseCase) produces Map(linkKey ->expectedForEngineWithUseCaseLink)
    (rc, useCase1) produces Map(linkKey ->expectedForUseCase1Link)
    (rc, scenario1) produces Map(linkKey -> expectedForScenario1Link)
    (rc, scenario2) produces Map(linkKey ->expectedForScenario2Link)
    (rc, scenario3) produces Map(linkKey ->expectedForScenario3Link)
  }

  val findChildren = new Engine[EngineComponent[_, _], List[EngineComponent[_, _]]] {
    engineWithUseCase produces List(useCase1) because { case e: Engine[_, _] => e.asUseCase.components }
    emptyEngine produces List()
    useCase1 produces List(scenario1, scenario2, scenario3) because { case uc: UseCase[_, _] => uc.components.reverse }
    scenario1 produces List() because { case s: Scenario[_, _] => List() }
  }
  val findTypeName = new Engine[EngineComponent[_, _], String] {
    engineWithUseCase produces engineTypeName because { case e: Engine[_, _] => engineTypeName }
    useCase1 produces useCaseTypeName because { case u: UseCase[_, _] => useCaseTypeName }
    scenario1 produces scenarioTypeName because { case s: Scenario[_, _] => scenarioTypeName }
  }
  val render = new Engine2[RenderContext, EngineComponent[_, _], Any]("Produces the maps for the rendering template story") {
    (rc, scenario1) produces expectedForScenario1 byRecursion {
      case (engine, (rc, ec)) => Map(
        idKey -> rc.path(ec),
        typeKey -> findTypeName(ec),
        titleKey -> ec.title,
        linkKey -> makeLink(rc, ec),
        childrenKey -> findChildren(ec).map(c => Map(findTypeName(c) -> engine(rc, c)))
      )
    }
    (rc, scenario2) produces expectedForScenario2
    (rc, scenario3) produces expectedForScenario3
//    (rc, engineWithUseCase) produces expectedForEngineWithUseCase
    (rc, useCase1) produces expectedForUseCase1
  }
}
