package org.cddcore.rendering

import org.cddcore.engine.AbstractEngine
import org.cddcore.engine.enginecomponents.{EngineComponent, UseCase}


object PathMap extends TestObjectsForRendering {
  type EC = EngineComponent[_, _]

  def apply(ecs: List[EC]): PathMap = new PathMap(add(List(), Map(), ecs))
  def apply(ecs: EC*): PathMap = apply(ecs.toList)

  def add(fragments: List[Int], map: Map[EC, String], ecs: List[EngineComponent[_, _]]): Map[EC, String] = {
    ecs.reverse.zipWithIndex.foldLeft(map) { case (acc, (ec, i)) =>
      val newFragments = fragments :+ (i + 1)
      val path = (newFragments).mkString(".")
      val withThis = acc + (ec -> path)
//      println("Adding ", path, ec)
      ec match {
        case engine: AbstractEngine[_, _] => add(newFragments, withThis, engine.asUseCase.components)
        case holder: UseCase[_, _] => add(newFragments, withThis, holder.components)
        case _ => withThis
      }
    }
  }
}


class PathMap(map: Map[EngineComponent[_, _], String]) {
  def apply(ec: EngineComponent[_, _]) = map(ec)
}