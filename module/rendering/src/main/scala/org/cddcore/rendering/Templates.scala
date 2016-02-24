package org.cddcore.rendering

import java.io.{OutputStreamWriter, StringReader}
import java.util
import java.util.Date

import com.github.mustachejava.{ObjectHandler, DefaultMustacheFactory}
import com.twitter.mustache.ScalaObjectHandler
import org.cddcore.engine.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.engine.{Engine, Engine2}

import scala.collection.convert.WrapAsJava

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

  val scenariosKey = "scenarios"
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

  def apply(name: String, template: String) = mf.compile(new StringReader(template), name)

  def apply(name: String) = mf.compile(name)
}

trait MustacheTemplates extends KeysForRendering {

  //
  //  val linkTemplate = Mustache("link", "{{#link}}<a href='{{linkUrl}}' alt='{{title}}'>{{title}}<img src='{{iconUrl}}' /></a>{{/link}}")
  //  val scenarioFullTemplate = Mustache("scenarioFull",
  //    """<div id='{{id}}' class='scenario'><div class='scenarioTitle'>{{title}} {{>link}}</div><!-- scenarioTitle -->
  //      |   <table class='scenarioTable'">{{#link}}{{>link}}{{/link}}
  //      |      <tr><td>Situation</td><td>{{situation}}</td></tr>
  //      |      <tr><td>Expected</td><td>{{expected}}</td></tr>
  //      |   </table>
  //      |</div><!-- scenario -->""".stripMargin)
  //
  //  val useCaseFullTemplate = Mustache("useCaseFull",
  //    ""nt{{/chil"<div id='{{ id }}' class='usecase'>
  //      |    <div class='usecaseTitle'> {{title}} {{> link}} </div><!-- usecaseTitle -->
  //      |    {{# children}} > componedren}}
  //      |</div><!-- usecase' --> """.stripMargin)
  //
  //  val engineFullTemplate = Mustache("engineFull",
  //    """<div id='{{id}}' class='engine'>
  //      |    <div class='engineTitle'> {{ title }} {{>link}} </div><!-- engineTitle -->
  //      |    {{#children}}{{>component}}{{/children}}
  //      |</div><!-- engine' --> """.stripMargin)
  //
  //  val childrenTemplate = Mustache("children",
  //    """CHILDREN {{#UseCase}}INSIDE USECASE{{.}}{{>UseCase}}{{/UseCase}}
  //      |{{#Scenario}}{{>Scenario}}{{/Scenario}}""".stripMargin)
  //
  //  val partials = Map(
  //    engineTypeName -> engineFullTemplate,
  //    useCaseTypeName -> useCaseFullTemplate,
  //    scenarioTypeName -> scenarioFullTemplate,
  //    linkKey -> linkTemplate,
  //    "component" -> childrenTemplate)
}

trait ExpectedForTemplates extends TestObjectsForRendering with KeysForRendering with Icons {
  protected val expectedForScenario1Link = Map(titleKey -> scenario1.title, linkUrlKey -> rc.url(scenario1), iconUrlKey -> scenarioIcon)
  protected val expectedForScenario2Link = Map(titleKey -> scenario2.title, linkUrlKey -> rc.url(scenario2), iconUrlKey -> scenarioIcon)
  protected val expectedForScenario3Link = Map(titleKey -> scenario3.title, linkUrlKey -> rc.url(scenario3), iconUrlKey -> scenarioIcon)
  protected val expectedForUseCase1Link = Map(titleKey -> useCase1.title, linkUrlKey -> rc.url(useCase1), iconUrlKey -> useCasesIcon)
  protected val expectedForEngineWithUseCaseLink = Map(titleKey -> engineWithUseCase.title, linkUrlKey -> rc.url(engineWithUseCase), iconUrlKey -> engineWithTestsIcon)

  protected val emptyUsecasesAndScenarios = Map(scenariosKey -> List(), useCasesKey -> List())

  protected val expectedForScenario1Depth0 = Map(
    idKey -> rc.path(scenario1),
    typeKey -> scenarioTypeName,
    titleKey -> scenario1.title,
    linkKey -> expectedForScenario1Link)


  protected val expectedForScenario2Depth0 = Map(
    idKey -> rc.path(scenario2),
    typeKey -> scenarioTypeName,
    titleKey -> scenario2.title,
    linkKey -> expectedForScenario2Link)


  protected val expectedForScenario3Depth0 = Map(
    idKey -> rc.path(scenario3),
    typeKey -> scenarioTypeName,
    titleKey -> scenario3.title,
    linkKey -> expectedForScenario3Link)


  protected val expectedForUseCase1Depth0 = Map(
    idKey -> rc.path(useCase1),
    typeKey -> useCaseTypeName,
    titleKey -> "someUseCase",
    linkKey -> expectedForUseCase1Link)

  protected val expectedForEngineDepth0 = Map(
    idKey -> rc.path(engineWithUseCase),
    typeKey -> engineTypeName,
    titleKey -> engineWithUseCase.title,
    linkKey -> expectedForEngineWithUseCaseLink)

  protected val expectedForUseCase1Depth1 = expectedForUseCase1Depth0 ++
    Map(scenariosKey -> List(expectedForScenario1Depth0, expectedForScenario2Depth0, expectedForScenario3Depth0),
      useCasesKey -> List())

  protected val expectedForEngineWithUseCaseDepth1 = expectedForEngineDepth0 ++ Map(
    useCasesKey -> List(expectedForUseCase1Depth0),
    scenariosKey -> List()
  )
  protected val expectedForEngineWithUseCase = expectedForEngineDepth0 ++ Map(
    useCasesKey -> List(expectedForUseCase1Depth0),
    scenariosKey -> List()
  )

  protected val expectedForScenario1Depth1 = expectedForScenario1Depth0 ++ emptyUsecasesAndScenarios
  protected val expectedForScenario2Depth1 = expectedForScenario2Depth0 ++ emptyUsecasesAndScenarios
  protected val expectedForScenario3Depth1 = expectedForScenario3Depth0 ++ emptyUsecasesAndScenarios
  protected val expectedForEngineWithUseCaseDepth0 = expectedForUseCase1Depth0 ++ emptyUsecasesAndScenarios

}

object Templates extends TestObjectsForRendering with Icons with KeysForRendering with ExpectedForTemplates with MustacheTemplates {

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
    (rc, engineWithUseCase) produces Map(linkKey -> expectedForEngineWithUseCaseLink)
    (rc, useCase1) produces Map(linkKey -> expectedForUseCase1Link)
    (rc, scenario1) produces Map(linkKey -> expectedForScenario1Link)
    (rc, scenario2) produces Map(linkKey -> expectedForScenario2Link)
    (rc, scenario3) produces Map(linkKey -> expectedForScenario3Link)
  }

  private def ucAndS(uc: List[EngineComponent[_, _]], s: List[EngineComponent[_, _]]) = Map(useCasesKey -> uc, scenariosKey -> s)

  private def partition(ecs: List[EngineComponent[_, _]]) =
    ucAndS(ecs.filter(_.isInstanceOf[UseCase[_, _]]), ecs.filter(_.isInstanceOf[Scenario[_, _]]))

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

  object renderDepth0 extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, useCase1) produces Map(
      idKey -> rc.path(useCase1),
      typeKey -> useCaseTypeName,
      titleKey -> useCase1.title,
      linkKey -> expectedForUseCase1Link) byRecursion { case (engine, (rc, ec)) =>
      makeLink(rc, ec) ++ Map(
        idKey -> rc.path(ec),
        typeKey -> findTypeName(ec),
        titleKey -> ec.title)
    }
    (rc, engineWithUseCase) produces Map(
      idKey -> rc.path(engineWithUseCase),
      typeKey -> engineTypeName,
      titleKey -> engineWithUseCase.title,
      linkKey -> expectedForEngineWithUseCaseLink
    )

    (rc, scenario1) produces Map(idKey -> rc.path(scenario1),
      typeKey -> scenarioTypeName,
      titleKey -> scenario1.title,
      linkKey -> expectedForScenario1Link)

    (rc, scenario2) produces Map(
      idKey -> rc.path(scenario2),
      typeKey -> scenarioTypeName,
      titleKey -> scenario2.title,
      linkKey -> expectedForScenario2Link)

  }

  object renderDepth1 extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, useCase1) produces expectedForUseCase1Depth1 by { case (rc, ec) =>
      val children = findChildren(ec)
      makeLink(rc, ec) ++ Map(
        idKey -> rc.path(ec),
        typeKey -> findTypeName(ec),
        titleKey -> ec.title,
        scenariosKey -> children.filter(_.isInstanceOf[Scenario[_, _]]).map(renderDepth0(rc, _)),
        useCasesKey -> children.filter(_.isInstanceOf[UseCase[_, _]]).map(renderDepth0(rc, _)))
    }
    (rc, scenario1) produces expectedForScenario1Depth1
    (rc, scenario2) produces expectedForScenario2Depth1
    (rc, scenario3) produces expectedForScenario3Depth1

  }
//
//  val render = new Engine2[RenderContext, EngineComponent[_, _], Map[String, Any]]("Produces the maps for the rendering template story") {
//    (rc, scenario1) produces expectedForScenario1Depth1 byRecursion { case (engine, (rc, ec)) => {
//      val children = findChildren(ec)
//      makeLink(rc, ec) ++ Map(
//        idKey -> rc.path(ec),
//        typeKey -> findTypeName(ec),
//        titleKey -> ec.title,
//        scenariosKey -> children.filter(_.isInstanceOf[Scenario[_, _]]).map(engine(rc, _)),
//        useCasesKey -> children.filter(_.isInstanceOf[UseCase[_, _]]).map(engine(rc, _)))
//
//    }
//    }
//    (rc, scenario2) produces expectedForScenario2Depth0
//    (rc, scenario3) produces expectedForScenario3Depth0
//    (rc, useCase1) produces expectedForUseCase1Depth0
//    (rc, engineWithUseCase) produces expectedForEngineWithUseCaseDepth0
//  }

  def forMustache(s: Any): Any = s match {
    case m: Map[String, _] => m.foldLeft(new util.HashMap[String, Any]) { case (acc, (k, v)) => acc.put(k, forMustache(v)); acc }
    case l: List[_] => l.foldLeft(new util.ArrayList[Any]) { (acc, v) => acc.add(forMustache(v)); acc }
    case _ => s
  }

  //  def main(args: Array[String]) {
  //    val writer = new OutputStreamWriter(System.out)
  //    Mustache("link.mustache").execute(writer, forMustache(Map(linkKey -> expectedForScenario1Link)))
  //    writer.write("\n\n")
  //    writer.flush
  //    Mustache("Scenario.mustache").execute(writer, forMustache(expectedForScenario1))
  //    writer.write("\n\n")
  //    writer.write("\n\n")
  //    writer.flush
  //    Mustache("UseCase.mustache").execute(writer, forMustache(expectedForUseCase1))
  //    writer.flush();
  //    println
  //    println
  //    Mustache("Engine.mustache").execute(writer, forMustache(expectedForEngineWithUseCase))
  //    writer.flush();
  //    println
  //    println
  //    println
  //    Mustache("Engine.mustache").execute(writer, forMustache(render(rc, engineWithUseCase)))
  //    writer.flush();
  //    println
  //    //    //    println(expectedForEngineWithUseCase)
  //    //    //    engineFullTemplate.execute(writer, expectedForEngineWithUseCase)
  //
  //  }

}
