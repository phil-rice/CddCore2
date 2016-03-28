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
