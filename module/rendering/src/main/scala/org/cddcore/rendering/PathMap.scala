package org.cddcore.rendering

import org.cddcore.engine.{AbstractEngine, Engine}
import org.cddcore.enginecomponents.{EngineComponent, UseCase}
import org.cddcore.utilities.{MapOfLists, Strings}


object PathMap extends TestObjectsForRendering {
  type EC = EngineComponent[_, _]

  def checkSufficientlyUniqueTitles(ecs: Traversable[EC]) = {
    import MapOfLists._
    val names = ecs.foldLeft(Map[String, List[String]]())((acc, ec) => acc addToList (Strings.cleanString(ec.title) -> ec.title))
    val duplicates = names.filter { case (clean, list) => list.size > 1 }
    if (duplicates.size > 0) throw new DuplicateTitleException(duplicates)
  }

  def apply(ecs: Traversable[Engine[_, _]]): PathMap = {
    checkSufficientlyUniqueTitles(ecs)
    val map = ecs.foldLeft(Map[EC, String]()) { (acc, ec) =>
      val cleanTitle = Strings.cleanString(ec.title)
      acc ++ add(Strings.cleanString(ec.title), List(), Map(), ec.asUseCase.components) + (ec -> (cleanTitle + "/index"))
    }
    new PathMap(map)
  }

  def apply(ecs: Engine[_, _]*): PathMap = apply(ecs.toList)

  def add(root: String, fragments: List[Int], map: Map[EC, String], ecs: List[EngineComponent[_, _]]): Map[EC, String] = {
    ecs.reverse.zipWithIndex.foldLeft(map) { case (acc, (ec, i)) =>
      val newFragments = fragments :+ (i + 1)
      val path = root + "/" + (newFragments).mkString(".")
      val withThis = acc + (ec -> path)
      //      println("Adding ", path, ec)
      ec match {
        case engine: AbstractEngine[_, _] => add(root, newFragments, withThis, engine.asUseCase.components)
        case holder: UseCase[_, _] => add(root, newFragments, withThis, holder.components)
        case _ => withThis
      }
    }
  }
}


class PathMap(map: Map[EngineComponent[_, _], String]) {
  lazy val inversePathMap = map.foldLeft(Map[String, EngineComponent[_, _]]()) { case (acc, (k, v)) => acc + (v -> k) }

  def apply(ec: EngineComponent[_, _]) = map(ec)

}