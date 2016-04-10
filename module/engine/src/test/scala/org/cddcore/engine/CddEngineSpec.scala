/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.{CddSpec, ChildLifeCycle}


trait CddEngineSpec extends CddSpec {

  def mapErrorsToClassName[K](errors: Map[K, Exception]) = errors.map { case (k, e) => (k, e.getClass.getSimpleName) }

  def toErrors[P, R](block: ChildLifeCycle[EngineComponent[P, R]] => Scenario[P, R]) = {
    var remembered: Option[Scenario[P, R]] = None
    val e = new Engine[P, R] {
      remembered = Some(block(childLifeCycle))
    }
    (e, remembered.get, mapErrorsToClassName(e.hierarchyBuilder.holder.errors))
  }
}

trait CddNonRecursiveSpec[P, R] extends CddEngineSpec {
  def mockEngine[P, R]: P => R = _ => throw new RuntimeException("Should not try anything recursive in this test")
}
