/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.rendering

import java.nio.file.Files

import org.cddcore.engine.{Engine, Trace}
import org.cddcore.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.utilities.DisplayProcessor

case class TemplateNames(engineName: String, useCaseName: String, scenarioName: String) {
  def nameFor(ec: EngineComponent[_, _]) =
    ec match {
      case _: Engine[_, _] => engineName
      case _: UseCase[_, _] => useCaseName
      case _: Scenario[_, _] => scenarioName
    }
}

class EnginePimper[P, R](ec: Engine[P, R]) {

  def renderContext(implicit renderConfiguration: RenderConfiguration, displayProcessor: DisplayProcessor) = Renderer.renderContext((ec))

  def toHtml(implicit rc: RenderContext = renderContext) = Renderer.toHtml(ec)
}

class EngineComponentPimper[P, R](ec: EngineComponent[P, R]) {
  type EC = EngineComponent[P, R]

  def toMap(implicit renderContext: RenderContext) = Renderer.scalaMap(ec)

  def withChildrenPaths(implicit renderContext: RenderContext): List[List[EC]] = withChildrenPaths(ec, List())

  protected def withChildrenPaths(ec: EngineComponent[P, R], path: List[EC])(implicit renderContext: RenderContext): List[List[EC]] = {
    val thisPath = ec :: path
    thisPath :: Templates.findChildren(ec).asInstanceOf[List[EC]].flatMap(withChildrenPaths(_, thisPath))
  }

  def withDescendents = Renderer.withDescendents(ec)

  def withChildrenPathMaps(implicit renderContext: RenderContext) = withChildrenPaths.map(path => path.map { case ec: EC => Renderer.scalaMap(ec) })
}


