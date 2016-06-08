/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import java.io._
import java.util.Date

import org.cddcore.engine._
import org.cddcore.enginecomponents._
import org.cddcore.utilities.{DisplayProcessor, Strings}

trait ReferenceMapMakers {
  def documentToMap(d: Document) = Map("name" -> d.name, "ref" -> d.ref)

  def referenceToMap(rc: RenderContext)(r: Reference) = Map("document" -> documentToMap(r.document), "internalRef" -> r.internalRef, "imgSrc" -> (rc.urlBase + "images/document.png"))
}

object Icons {
  val engineWithTestsIcon = "images/engine.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png"
  val useCasesIcon = "images/usecase.png"
  //http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png"
  val scenarioIcon = "images/scenario.png"
  //http://imagizer.imageshack.us/a/img537/7868/P3Ucx2.png"
  val errorScenarioIcon = "images/errorScenario.png" //http://imagizer.imageshack.us/a/img537/7868/P3Ucx2.png"
}

object Templates extends TestObjectsForRendering with KeysForRendering with ExpectedForTemplates {

  import Icons._
  import Strings.uri

  val findIconUrl = new Engine2[RenderContext, EngineComponent[_, _], String] {
    (rc, emptyEngine) produces expectedEngineIcon because { case (rc, e: Engine[_, _]) => uri(rc.referenceFilesUrlBase, engineWithTestsIcon) }
    (rc, engineWithUseCase) produces expectedEngineIcon
    (rc, useCase1) produces expectedUsecaseIcon because { case (rc, _: UseCase[_, _]) => uri(rc.referenceFilesUrlBase, useCasesIcon) }
    (rc, scenario1) produces expectedErrorScenarioIcon because { case (rc, s: Scenario[_, _]) if rc.exceptions.contains(s) => uri(rc.referenceFilesUrlBase, errorScenarioIcon) }
    (rc, scenario2) produces expectedScenarioIcon because { case (rc, s: Scenario[_, _]) => uri(rc.referenceFilesUrlBase, scenarioIcon) }
    (rc, scenario3) produces expectedScenarioIcon
  }

  val makeLink = new Engine2[RenderContext, EngineComponent[_, _], Map[String, _]]("Produces the maps for a link to a component") {
    (rc, engineWithUseCase) produces linkForEngine by {
      case (rc, ec) => Map(titleKey -> ec.title, linkUrlKey -> rc.url(ec), iconUrlKey -> findIconUrl(rc, ec), definedAtKey -> ec.definedInSourceCodeAt.toString)
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
    summaryKey -> rc.displayProcessor.summary(ec),
    titleKey -> ec.title
  )

  def renderScenario(rc: RenderContext, s: Scenario[_, _]) = {
    val raw = Map(
      idKey -> rc.idPath(s),
      typeKey -> findTypeName(s),
      linkKey -> makeLink(rc, s),
      titleKey -> s.title,
      summaryKey -> rc.displayProcessor.summary(s),
      commentKey -> s.comment.getOrElse(""),
      situationKey -> rc.displayProcessor.html(s.situation),
      expectedKey -> s.expectedOption.map(expected => rc.displayProcessor.html(expected)).getOrElse("<Not Known>"),
      referencesKey -> s.references.map(referenceToMap(rc)))
    rc.exceptions.get(s).fold(raw) {
      case ha: HasActual[_] => raw + ("actual" -> rc.displayProcessor.html(ha.actual))
      case r: ReasonInvalidException[_, _] => raw + ("reason" -> r.scenario.reason.prettyDescription)
      case _ => raw
    }
  }

  def exceptionMap(e: Exception) = {
    val raw = Map("message" -> e.getMessage, "class" -> e.getClass.getSimpleName, "stack" -> e.getStackTrace.take(5).mkString("\n"))
    e match {
      case wa: WithAdvice => raw + ("advice" -> wa.advice)
      case _ => raw
    }
  }

  object renderData extends Engine2[RenderContext, EngineComponent[_, _], Map[String, _]] {
    (rc, engineWithUseCase) produces dataForEngine because { case (rc, e: Engine[_, _]) => renderRawData(rc, e) ++ Map(referencesKey -> e.references.map(referenceToMap(rc))) }

    (rc, useCase1) produces dataForUseCase1 because { case (rc, uc: UseCase[_, _]) => renderRawData(rc, uc) ++ Map(commentKey -> uc.comment.getOrElse(""), referencesKey -> uc.references.map(referenceToMap(rc))) }
    (rc, useCase2) produces dataForUseCase2

    (rc, scenario1) produces dataForScenario1 + ("error" -> exceptionMap(rc.exceptions(scenario1))) because {
      case (rc, s: Scenario[_, _]) if rc.exceptions.contains(s) => renderScenario(rc, s) + ("error" -> exceptionMap(rc.exceptions(s)))
    }

    (rc, scenario2) produces dataForScenario2 because { case (rc, s: Scenario[_, _]) => renderScenario(rc, s) }
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
    Map(titleKey -> path.head.title,
      summaryKey -> rc.displayProcessor.summary(path.head),
      "definedAt" -> path.head.definedInSourceCodeAt.toString,
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
