/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import scala.util.Try

trait Trace {
  def startTime: Long

  def duration: Long

  def children: List[Trace]
}

case class TraceEngine[P, R](startTime: Long, duration: Long, engine: AbstractEngine[P, R], conclusion: ConclusionNode[P, R], params: P, result: R, children: List[Trace]) extends Trace {
  override def hashCode = engine.hashCode() / 4 + conclusion.hashCode() / 4 + params.hashCode() / 4 + result.hashCode() / 4

  override def equals(other: Any) = other match {
    case t: TraceEngine[P, R] => t.engine == engine && t.conclusion == conclusion && t.params == params && t.result == result && t.children == children
  }

  override def toString = f"TraceEngine(duration=${duration / 1000}%5.2f,${engine.title}, conc=${conclusion.mainScenario},params=$params, result=$result, children(${children.size})=$children)"
}
