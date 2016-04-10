/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import org.cddcore.utilities.CddSpec

class RenderingSpec extends CddSpec with ExpectedForTemplates {

  import Renderer._

  "The withChildrenPaths" should "return the objects, and then the children with paths to the root" in {
    implicit val renderContext = engineWithUseCase.renderContext
    scenario1.withChildrenPaths shouldBe List(List(scenario1))
    scenario2.withChildrenPaths shouldBe List(List(scenario2))
//    println(Renderer.mapList(useCase1.withChildrenPaths, _.title))
    //
    useCase1.withChildrenPaths shouldBe List(
      List(useCase1),
      List(scenario1, useCase1), List(scenario2, useCase1))


    engineWithUseCase.withChildrenPaths shouldBe List(
      List(engineWithUseCase),
      List(useCase1, engineWithUseCase),
      List(scenario1, useCase1, engineWithUseCase),
      List(scenario2, useCase1, engineWithUseCase),
      List(useCase2, engineWithUseCase),
      List(scenario3, useCase2, engineWithUseCase),
      List(scenario4, engineWithUseCase)
    )
  }

  "RenderOne" should "validate" in {
    Templates.renderData.validate
  }

//  "RenderOneAndChild" should "validate" in {
//    Templates.renderDataAndLinks.validate
//  }
//
//  "withChildrenPathMaps" should "be" in {
//    implicit val dp = displayProcessorModifiedForSituations
//    implicit val renderDefintion = SimpleRenderConfiguration("urlBase")
//    implicit val rc = engineWithUseCase.renderContext
//
//    scenario1.withChildrenPathMaps shouldBe List(
//      List(dataForScenario1WithLinks))
//
//    useCase1.withChildrenPathMaps shouldBe List(
//      List(detailForUseCase1),
//      List(dataForScenario1WithLinks, detailForUseCase1),
//      List(dataForScenario2WithLinks, detailForUseCase1),
//      List(dataForScenario3Withlinks, detailForUseCase1))
//
//    engineWithUseCase.withChildrenPathMaps shouldBe List(
//      List(detailForEngine),
//      List(detailForUseCase1, detailForEngine),
//      List(dataForScenario1WithLinks, detailForUseCase1, detailForEngine),
//      List(dataForScenario2WithLinks, detailForUseCase1, detailForEngine),
//      List(dataForScenario3Withlinks, detailForUseCase1, detailForEngine)
//    )
//  }
//
//  "toSingleMap" should "be" in {
//    implicit val dp = displayProcessorModifiedForSituations
//    implicit val renderDefintion = SimpleRenderConfiguration("urlBase")
//    implicit val rc = engineWithUseCase.renderContext
//
//    scenario1.toSingleMaps shouldBe List(Map("id" -> "1.1.1", "Scenario" -> dataForScenario1WithLinks))
//
////    println(useCase1.toSingleMaps.mkString("\n\n\n"))
//
//    useCase1.toSingleMaps shouldBe List(
//      Map("id" -> "1.1", "UseCase" -> detailForUseCase1),
//      Map("id" -> "1.1.1", "Scenario" -> (detailForUseCase1 ++ Map("path" -> dataForScenario1WithLinks))),
//      Map("id" -> "1.1.2", "Scenario" -> (detailForUseCase1 ++ Map("path" -> dataForScenario2WithLinks))),
//      Map("id" -> "1.1.3", "Scenario" -> (detailForUseCase1 ++ Map("path" -> dataForScenario3Withlinks))))
//
//    engineWithUseCase.toSingleMaps shouldBe List(
//      Map("id" -> "1", "Engine" -> detailForEngine),
//      Map("id" -> "1.1", "UseCase" -> (detailForEngine ++ Map("path" -> detailForUseCase1))),
//      Map("id" -> "1.1.1", "Scenario" -> (detailForEngine ++ Map("path" -> (detailForUseCase1 ++ Map("path" -> dataForScenario1WithLinks))))),
//      Map("id" -> "1.1.2", "Scenario" -> (detailForEngine ++ Map("path" -> (detailForUseCase1 ++ Map("path" -> dataForScenario2WithLinks))))),
//      Map("id" -> "1.1.3", "Scenario" -> (detailForEngine ++ Map("path" -> (detailForUseCase1 ++ Map("path" -> dataForScenario3Withlinks))))))
//  }

}
