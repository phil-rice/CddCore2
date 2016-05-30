/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import java.util.Date

import org.cddcore.engine.{Engine, TraceEngine}
import org.cddcore.enginecomponents.{Document, Reference, Scenario, UseCase}
import org.cddcore.utilities.{DisplayProcessor, Strings}

trait TestObjectsForRendering {
  import Icons._
  val expectedEngineIcon = Strings.uri("refBase", engineWithTestsIcon)
  val expectedUsecaseIcon = Strings.uri("refBase", useCasesIcon)
  val expectedScenarioIcon = Strings.uri("refBase", scenarioIcon)
  val displayProcessorModifiedForSituations = DisplayProcessor.defaultDisplayProcessor.
    withSummarize { case (dp, x) => s"Summary($x)" }.
    withHtml { case (dp, x) => s"Html($x)" }.
    withDetailer { case (dp, x) => s"Detailer($x)" }
  protected val emptyEngine = new Engine[Int, String]("someEngineTitle") {}
  val d1 = Document.internet("someHRef1")
  val d2 = Document.internet("name2", "someHRef2")
  protected val engineWithUseCase = new Engine[Int, String]("someEngineTitle2", references = List(Reference(d2, "engineRef"))) {
    useCase("someUseCase") {
      1 produces "one" when (_ == 1)
      2 produces "two" when (_ == 2) ref d1
    }
    useCase("someOtherCase") {
      3 produces "three" when (_ == 3)
    }
    4 produces "four"
  }
  protected val engineNested = new Engine[Int, String]("engineNested") {
    1 produces "onetwo" by (engineWithUseCase(1) + engineWithUseCase(2))
  }
  protected val List(useCase1: UseCase[Int, String], useCase2: UseCase[Int, String], scenario4: Scenario[Int, String]) = engineWithUseCase.asUseCase.components.reverse
  protected val List(scenario1: Scenario[_, _], scenario2: Scenario[_, _]) = useCase1.components.reverse
  protected val List(scenario3: Scenario[_, _]) = useCase2.components.reverse


  protected val rc: RenderContext = RenderContext(new Date(), "urlBase", "refBase", PathMap(emptyEngine, engineWithUseCase, engineNested), new FileUrlManipulations())(displayProcessorModifiedForSituations)


  protected val cn1 = engineWithUseCase.decisionTree.conclusionNodeFor(engineWithUseCase, 1)
  protected val cn2 = engineWithUseCase.decisionTree.conclusionNodeFor(engineWithUseCase, 2)
  protected val cnParent = engineNested.decisionTree.conclusionNodeFor(engineWithUseCase, 1)
  protected val trace1 = TraceEngine(100, 20l, engineWithUseCase, cn1, 1, "one", List())
  protected val trace2 = TraceEngine(300, 400, engineWithUseCase, cn2, 2, "two", List())
  protected val parentTrace = TraceEngine(500, 600, engineNested, cnParent, 1, "onetwo", List(trace1, trace2))

  protected val pathThroughDTForTrace1 = engineWithUseCase.decisionTree.pathFor(engineWithUseCase, trace1.params)
  protected val pathThroughDTForTrace2 = engineWithUseCase.decisionTree.pathFor(engineWithUseCase, trace2.params)
  protected val pathThroughDTForParentTrace = engineNested.decisionTree.pathFor(engineNested, parentTrace.params)

}

trait ExpectedForTemplates extends TestObjectsForRendering with KeysForRendering with  ReferenceMapMakers {

  protected val linkForScenario1 = Map(titleKey -> scenario1.title, linkUrlKey -> rc.url(scenario1), iconUrlKey -> expectedScenarioIcon, definedAtKey -> scenario1.definedInSourceCodeAt)
  protected val linkForScenario2 = Map(titleKey -> scenario2.title, linkUrlKey -> rc.url(scenario2), iconUrlKey -> expectedScenarioIcon, definedAtKey -> scenario2.definedInSourceCodeAt)
  protected val linkForScenario3 = Map(titleKey -> scenario3.title, linkUrlKey -> rc.url(scenario3), iconUrlKey -> expectedScenarioIcon, definedAtKey -> scenario3.definedInSourceCodeAt)
  protected val linkForScenario4 = Map(titleKey -> scenario4.title, linkUrlKey -> rc.url(scenario4), iconUrlKey -> expectedScenarioIcon, definedAtKey -> scenario4.definedInSourceCodeAt)
  protected val linkForUseCase1 = Map(titleKey -> useCase1.title, linkUrlKey -> rc.url(useCase1), iconUrlKey -> expectedUsecaseIcon, definedAtKey -> useCase1.definedInSourceCodeAt)
  protected val linkForUseCase2 = Map(titleKey -> useCase2.title, linkUrlKey -> rc.url(useCase2), iconUrlKey -> expectedUsecaseIcon, definedAtKey -> useCase2.definedInSourceCodeAt)
  protected val linkForEngine = Map(titleKey -> engineWithUseCase.title, linkUrlKey -> rc.url(engineWithUseCase), iconUrlKey -> expectedEngineIcon, definedAtKey -> engineWithUseCase.definedInSourceCodeAt)

  protected val emptyUsecasesAndScenarios = Map(scenariosKey -> List(), useCasesKey -> List())

  protected val dataForScenario1 = Map(
    idKey -> rc.idPath(scenario1),
    typeKey -> scenarioTypeName,
    titleKey -> scenario1.title,
    linkKey -> linkForScenario1,
    commentKey -> "",
    situationKey -> "Html(1)",
    expectedKey -> "Html(one)",
    referencesKey -> scenario1.references.map(referenceToMap(rc)))


  protected val dataForScenario2 = Map(
    idKey -> rc.idPath(scenario2),
    typeKey -> scenarioTypeName,
    titleKey -> scenario2.title,
    linkKey -> linkForScenario2,
    commentKey -> "",
    situationKey -> "Html(2)",
    expectedKey -> "Html(two)",
    referencesKey -> scenario2.references.map(referenceToMap(rc)))


  protected val dataForScenario3 = Map(
    idKey -> rc.idPath(scenario3),
    typeKey -> scenarioTypeName,
    titleKey -> scenario3.title,
    linkKey -> linkForScenario3,
    commentKey -> "",
    situationKey -> "Html(3)",
    expectedKey -> "Html(three)",
    referencesKey -> scenario3.references.map(referenceToMap(rc)))

  protected val dataForScenario4 = Map(
    idKey -> rc.idPath(scenario4),
    typeKey -> scenarioTypeName,
    titleKey -> scenario4.title,
    linkKey -> linkForScenario4,
    commentKey -> "",
    situationKey -> "Html(4)",
    expectedKey -> "Html(four)",
    referencesKey -> scenario4.references.map(referenceToMap(rc)))

  protected val dataForUseCase1 = Map(
    idKey -> rc.idPath(useCase1),
    typeKey -> useCaseTypeName,
    titleKey -> "someUseCase",
    linkKey -> linkForUseCase1,
    commentKey -> "",
    scenariosIconsKey -> List(linkForScenario1, linkForScenario2),
    referencesKey -> useCase1.references.map(referenceToMap(rc)))

  protected val dataForUseCase2 = Map(
    idKey -> rc.idPath(useCase2),
    typeKey -> useCaseTypeName,
    titleKey -> "someOtherCase",
    linkKey -> linkForUseCase2,
    commentKey -> "",
    scenariosIconsKey -> List(linkForScenario3),
    referencesKey -> useCase2.references.map(referenceToMap(rc)))


  protected val dataForEngine = Map(
    idKey -> rc.idPath(engineWithUseCase),
    typeKey -> engineTypeName,
    titleKey -> engineWithUseCase.title,
    linkKey -> linkForEngine,
    scenariosIconsKey -> List(linkForScenario4),
    referencesKey -> engineWithUseCase.references.map(referenceToMap(rc)))

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
