package org.cddcore.rendering

import org.cddcore.utilities.CddSpec

class RenderingSpec extends CddSpec with ExpectedForTemplates {

  import Renderer._

  "The withChildrenPaths" should "return the objects, and then the children with paths to the root" in {
    scenario1.withChildrenPaths shouldBe List(List(scenario1))
    scenario2.withChildrenPaths shouldBe List(List(scenario2))
    println(Renderer.mapList(useCase1.withChildrenPaths, _.title))
    //
    useCase1.withChildrenPaths shouldBe List(
      List(useCase1),
      List(scenario1, useCase1), List(scenario2, useCase1), List(scenario3, useCase1))


    engineWithUseCase.withChildrenPaths shouldBe List(
      List(engineWithUseCase),
      List(useCase1, engineWithUseCase),
      List(scenario1, useCase1, engineWithUseCase),
      List(scenario2, useCase1, engineWithUseCase),
      List(scenario3, useCase1, engineWithUseCase)
    )
  }

  "RenderOne" should "validate" in {
    Templates.renderDepth0.validate
  }

  "RenderOneAndChild" should "validate" in {
    Templates.renderDepth1.validate
  }

  "withChildrenPathMaps" should "be" in {
    implicit val dp = displayProcessorModifiedForSituations
    implicit val renderDefintion = SimpleRenderConfiguration("urlBase")
    implicit val rc = engineWithUseCase.renderContext

    scenario1.withChildrenPathMaps shouldBe List(
      List(expectedForScenario1Depth1))

    useCase1.withChildrenPathMaps shouldBe List(
      List(expectedForUseCase1Depth1),
      List(expectedForScenario1Depth1, expectedForUseCase1Depth1),
      List(expectedForScenario2Depth1, expectedForUseCase1Depth1),
      List(expectedForScenario3Depth1, expectedForUseCase1Depth1))

    engineWithUseCase.withChildrenPathMaps shouldBe List(
      List(expectedForEngineWithUseCaseDepth1),
      List(expectedForUseCase1Depth1, expectedForEngineWithUseCaseDepth1),
      List(expectedForScenario1Depth1, expectedForUseCase1Depth1, expectedForEngineWithUseCaseDepth1),
      List(expectedForScenario2Depth1, expectedForUseCase1Depth1, expectedForEngineWithUseCaseDepth1),
      List(expectedForScenario3Depth1, expectedForUseCase1Depth1, expectedForEngineWithUseCaseDepth1)
    )
  }

  "toSingleMap" should "be" in {
    implicit val dp = displayProcessorModifiedForSituations
    implicit val renderDefintion = SimpleRenderConfiguration("urlBase")
    implicit val rc = engineWithUseCase.renderContext

    scenario1.toSingleMaps shouldBe List(Map("id" -> "1.1.1", "Scenario" -> expectedForScenario1Depth1))

//    println(useCase1.toSingleMaps.mkString("\n\n\n"))

    useCase1.toSingleMaps shouldBe List(
      Map("id" -> "1.1", "UseCase" -> expectedForUseCase1Depth1),
      Map("id" -> "1.1.1", "Scenario" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario1Depth1))),
      Map("id" -> "1.1.2", "Scenario" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario2Depth1))),
      Map("id" -> "1.1.3", "Scenario" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario3Depth1))))

    engineWithUseCase.toSingleMaps shouldBe List(
      Map("id" -> "1", "Engine" -> expectedForEngineWithUseCaseDepth1),
      Map("id" -> "1.1", "UseCase" -> (expectedForEngineWithUseCaseDepth1 ++ Map("path" -> expectedForUseCase1Depth1))),
      Map("id" -> "1.1.1", "Scenario" -> (expectedForEngineWithUseCaseDepth1 ++ Map("path" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario1Depth1))))),
      Map("id" -> "1.1.2", "Scenario" -> (expectedForEngineWithUseCaseDepth1 ++ Map("path" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario2Depth1))))),
      Map("id" -> "1.1.3", "Scenario" -> (expectedForEngineWithUseCaseDepth1 ++ Map("path" -> (expectedForUseCase1Depth1 ++ Map("path" -> expectedForScenario3Depth1))))))
  }

}
