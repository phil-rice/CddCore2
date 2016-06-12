/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.ChildLifeCycle

class RememberingLifeCycle[P, R] extends ChildLifeCycle[EngineComponent[P, R]] {
  type EC = EngineComponent[P, R]
  type S = Scenario[P, R]
  var created = List[EC]()
  var modified = List[String]()
  var errorStrings = List[String]()
  var errors = List[Exception]()
  var last: EC = null

  def asString(s: EC) = s match {
    case s: Scenario[P, R] => s.situation + "/" + s.assertion + "/" + s.reason.getClass.getSimpleName
  }

  def created(child: EC) = created = {
    last = child;
    created :+ child
  }

  def update[X <: EngineComponent[P, R]](newChild: => X): X = {
    try {
      val c = newChild
      modified = modified :+ asString(c)
      last = c
      c
    } catch {
      case e: Exception => childHasException(last, e); last.asInstanceOf[X]
    }
  }

  def childHasException(c: EngineComponent[P, R], exception: Exception) = {
    errorStrings = errorStrings :+ exception.getClass.getSimpleName + "/" + exception.getMessage
    errors = errors :+ exception
  }
}
