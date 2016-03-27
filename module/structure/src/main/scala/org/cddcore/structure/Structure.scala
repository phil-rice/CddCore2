package org.cddcore.structure

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method}

import org.cddcore.utilities.{Display, DontDisplay, Reflection, Strings}

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node, NodeSeq}


case class PathRoot[S](root: S, debug: Boolean)(implicit structure: Structure[S]) {
  def \(path: String) = PathRootAndSteps(root, List(PathStep(true, path)), debug)

  def \\(path: String) = PathRootAndSteps(root, List(PathStep(false, path)), debug)
}

case class PathStep(linked: Boolean, element: String) {
  override def toString() = (if (linked) """\""" else """\\""") + element
}

trait PathResultStrategy[S, A] {
  def resultToAggregate(result: Iterable[S]): Try[A]

}


case class PathResult[S, A, T](convertor: A => T, strategy: PathResultStrategy[S, A])

case class PathRootAndSteps[S: Structure](root: S, steps: List[PathStep], debug: Boolean) {
  def \(path: String) = PathRootAndSteps(root, steps :+ PathStep(true, path), debug)

  def \\(path: String) = PathRootAndSteps(root, steps :+ PathStep(false, path), debug)

  def \[A, T](result: PathResult[S, A, T]) = Path(this, result, debug)
}

case class Path[S: Structure, A, T](pathRootAndSteps: PathRootAndSteps[S], result: PathResult[S, A, T], debug: Boolean) {
  def apply(): T = get().get

  def get(): Try[T] = {
    val r = implicitly[Structure[S]].findResult(pathRootAndSteps)
    result.strategy.resultToAggregate(r).map(result.convertor)
  }
}


object Structure {

  implicit object XMLStructure extends Structure[Node] {

    def sToString(s: Node) = s match {
      case e: Elem => e.text
    }


    def findResult(pathRootAndSteps: PathRootAndSteps[Node]) = {
      import pathRootAndSteps._
      steps.foldLeft(root: NodeSeq) { case (acc, step) =>
        step.linked match {
          case true => acc \ step.element
          case false => acc \\ step.element
        }
      }
    }

    def structureAsResult(s: Node): NodeSeq = s

    def structureTitle = "xml"
  }

}

/** This will be implemented by 'Json' and by 'XML' and any other 'structures'
  * For XML S will be Elem, and Result will be NodeSeq
  * For JSON S will be object ,and Result will be a list of objects
  * */
trait Structure[S] {

  def findResult(pathRootAndSteps: PathRootAndSteps[S]): Iterable[S]

  def sToString(s: S): String

  def structureTitle: String

}


class Situation[S: ClassTag : Structure] {
  val structure = implicitly[Structure[S]]

  object AggregateStrings extends PathResultStrategy[S, String] {
    def resultToAggregate(result: Iterable[S]) = Success(result.foldLeft(StringBuilder.newBuilder)((acc, s) => acc.append(structure.sToString(s))).toString())
  }

  object AggregateOptionString extends PathResultStrategy[S, Option[String]] {
    def resultToAggregate(result: Iterable[S]): Try[Option[String]] =
      if (result.isEmpty) Success(None) else AggregateStrings.resultToAggregate(result).map(Some(_))
  }

  object OneAndOnlyOneString extends PathResultStrategy[S, String] {
    def resultToAggregate(result: Iterable[S]): Try[String] = {
      result.toList match {
        case h :: Nil => AggregateStrings.resultToAggregate(result)
        case Nil => throw new IllegalStateException("Expected one value, got none")
        case l => throw new IllegalStateException(s"Expected one value, got ${l.size} which are ${Strings.oneLine(l.map(structure.sToString).mkString(","))}")
      }
    }
  }


  def root(s: S, debug: Boolean = false) = PathRoot(s, debug)

  def customPathResult[A, X](fn: A => X, strategy: PathResultStrategy[S, A]) = PathResult(fn, strategy)

  def string = PathResult((s: String) => s, AggregateStrings)

  def optString = PathResult((option: Option[String]) => option, AggregateOptionString)

  def int = PathResult((s: String) => s.toInt, AggregateStrings)

  object Fold {

    class FoldStrategy[Acc, A](mapFn: S => A, initialValue: => Acc, foldFn: (Acc, A) => Acc) extends PathResultStrategy[S, Acc] {
      def resultToAggregate(result: Iterable[S]): Try[Acc] = Try {
        result.map(mapFn).foldLeft(initialValue)(foldFn)
      }
    }

    def fold[Acc, A](mapFn: S => A)(initialValue: => Acc)(foldFn: (Acc, A) => Acc) =
      PathResult((acc: Acc) => acc, new FoldStrategy[Acc, A](mapFn, initialValue, foldFn))

    def string = fold[String, String](structure.sToString) _

    def int = fold[Int, Int](s => structure.sToString(s).toInt) _

    def double = fold[Double, Double](s => structure.sToString(s).toDouble) _

    def list[A](mapFn: S => A) = fold[List[A], A](mapFn)(List())((acc, a) => acc :+ a)

    def reverseList[A](mapFn: S => A) = fold[List[A], A](mapFn)(List())((acc, a) => a :: acc)

    def vector[A](mapFn: S => A) = fold[Vector[A], A](mapFn)(Vector())((acc, a) => acc :+ a)

    def set[A](mapFn: S => A) = fold[Set[A], A](mapFn)(Set())((acc, a) => acc + a)
  }

  lazy val reflection = Reflection(this)

  import Reflection._

  private type FieldMap = Map[Field, Try[Any]]

  lazy val fieldMapForDisplay: FieldMap = {
    val displayFieldMap: FieldMap = reflection.fieldMapForAnnotation[Display]
    val pathFieldMap: FieldMap = reflection.fieldMap[Path[S, Any, Any]].map { case (field, tryPath) =>
      val value = tryPath.flatMap(_.get).recover { case e => s"<error evaluating path>${e.getClass.getSimpleName}/${e.getMessage}" }
      (field, value.map(Strings.oneLine))
    }
    val unfinishedFieldMap: FieldMap =
      reflection.fieldMap[PathRootAndSteps[S]].map { case (field, v) => (field, Success("No Convertor")) } ++
        reflection.fieldMap[PathRoot[S]].map { case (field, v) => (field, Success("No Convertor")) }
    (Map[Field, Try[Any]]() ++ displayFieldMap ++ pathFieldMap ++ unfinishedFieldMap).removeAnnotationFromFieldMap[DontDisplay]
  }


  lazy val structureMap = reflection.fieldMap[S].removeAnnotationFromFieldMap[DontDisplay].sorted(reflection.allFields)

  override def toString = {
    val paths = fieldMapForDisplay.size match {
      case 0 => ""
      case _ => "\r\n  " + fieldMapForDisplay.displayStringMap(Strings.oneLine).sorted(reflection.allFields).displayString("\r\n  ")
    }
    val structureMapsAsString = "  " + structureMap.displayStringMap(Strings.oneLine).sorted(reflection.allFields).displayString("\r\n  ")
    val structures = structureMap.size match {
      case 0 => ""
      case 1 => structure.structureTitle + "\r\n" + structureMapsAsString
      case _ => structure.structureTitle + "s\r\n" + structureMapsAsString
    }
    s"${
      getClass.getSimpleName
    }($paths\r\n$structures)"
  }
}

class Xml extends Situation[Node]
