package org.cddcore.structure

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method}

import org.cddcore.utilities.{Reflection, Strings}

import scala.reflect.ClassTag
import scala.xml.{Node, NodeSeq}

/** This will be implemented by 'Json' and by 'XML' and any other 'structures'
  * For XML S will be Elem, and Result will be NodeSeq
  * */
trait Structure[S, Result] {

  def findResult(pathRootAndSteps: PathRootAndSteps[S, Result]): Result

  def structureAsResult(s: S): Result

}

case class PathRoot[S, Result](root: S)(implicit structure: Structure[S, Result]) {
  def \(path: String) = PathRootAndSteps(root, List(PathStep(true, path)))

  def \\(path: String) = PathRootAndSteps(root, List(PathStep(false, path)))
}

case class PathStep(linked: Boolean, element: String) {
  override def toString() = (if (linked) """\""" else """\\""") + element
}

case class PathResult[Result, T](convertor: Result => T)

case class PathRootAndSteps[S, Result](root: S, steps: List[PathStep])(implicit structure: Structure[S, Result]) {
  def \(path: String) = PathRootAndSteps(root, steps :+ PathStep(true, path))

  def \\(path: String) = PathRootAndSteps(root, steps :+ PathStep(false, path))

  def \[T](result: PathResult[Result, T]) = Path(this, result)
}

case class Path[S, Result, T](pathRootAndSteps: PathRootAndSteps[S, Result], result: PathResult[Result, T])(implicit structure: Structure[S, Result]) {
  def apply(): T = {
    val r = structure.findResult(pathRootAndSteps)
    result.convertor(r)
  }
}

abstract class StructureHolder[S: ClassTag, Result] {
  implicit def structure: Structure[S, Result]

  protected def structureTitle: String

  lazy val reflection = Reflection(this)
  lazy val fieldMapForDisplay = {
    val displayFieldMap = reflection.fieldMapForAnnotation[Display]
    val pathFieldMap = reflection.fieldMap[Path[S, Result, Any]].map { case (field, v) => (field, v()) }
    val unfinishedFieldMap =
      reflection.fieldMap[PathRootAndSteps[S, Any]].map { case (field, v) => (field, "No Convertor") } ++
      reflection.fieldMap[PathRoot[S, Any]].map { case (field, v) => (field, "No Convertor") }

    Map[Field, Any]() ++ displayFieldMap ++ pathFieldMap ++ unfinishedFieldMap
  }

  lazy val structureMap = reflection.fieldMap[S]


  override def toString = {

    val paths = fieldMapForDisplay.size match {
      case 0 => ""
      case _ => "\r\n  " + reflection.fieldMapToString(fieldMapForDisplay, Strings.oneLine, "\r\n  ")
    }
    val structureMapsAsString = "  " + reflection.fieldMapToString(structureMap, Strings.oneLine, "\r\n  ")
    val structures = structureMap.size match {
      case 0 => ""
      case 1 => structureTitle + "\r\n" + structureMapsAsString
      case _ => structureTitle + "s\r\n" + structureMapsAsString
    }
    s"${getClass.getSimpleName}($paths\r\n$structures)"
  }
}
