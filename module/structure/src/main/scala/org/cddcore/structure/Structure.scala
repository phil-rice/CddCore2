/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.structure

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method}

import org.cddcore.utilities._

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node, NodeSeq}



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

class Situation[S: ClassTag](implicit val structure: Structure[S]) extends PathResults[S] with Displayable {

  def structureTitle = getClass.getSimpleName

  lazy val reflection = Reflection(this)

  import Reflection._

  private type FieldMap = ListMap[Field, Try[Any]]

  private lazy val fieldMapForSummary: FieldMap = reflection.fieldMapForAnnotation[Summary]

  private lazy val fieldMapForDisplay: FieldMap = {
    val displayFieldMap: FieldMap = reflection.fieldMapForAnnotation[Display]
    val pathFieldMap: FieldMap = reflection.fieldMap[Path[S, Any, Any]].map {
      case (field, tryPath) =>
        val value = tryPath.flatMap(_.get).recover {
          case e => s"<error evaluating path>${
            e.getClass.getSimpleName
          }/${
            e.getMessage
          }"
        }
        (field, value.map(Strings.oneLine))
    }
    val unfinishedFieldMap: FieldMap =
      reflection.fieldMap[PathRootAndSteps[S]].map {
        case (field, v) => (field, Success("No Convertor"))
      } ++
        reflection.fieldMap[PathRoot[S]].map {
          case (field, v) => (field, Success("No Convertor"))
        }
    (ListMap[Field, Try[Any]]() ++ displayFieldMap ++ pathFieldMap ++ unfinishedFieldMap).removeAnnotationFromFieldMap[DontDisplay]
  }

  private lazy val structureMap = reflection.fieldMap[S].removeAnnotationFromFieldMap[DontDisplay].sorted(reflection.allFields)

  override def toString = detailed

  override def html(implicit dp: DisplayProcessor): String = detailed

  private def defaultSummary(implicit dp: DisplayProcessor): String =
    fieldMapForDisplay.toList match {
      case (field, Success(head)) :: _ => s"${getClass.getSimpleName}(${field.getName} -> ${dp.summary(head)})"
      case (field, Failure(head)) :: _ => dp.summary(head)
      case _ => toString.take(100)
    }

  override def summary(implicit dp: DisplayProcessor): String = {
    if (fieldMapForSummary.size == 0)
      defaultSummary
    else
      fieldMapForSummary.displayStringMap {
        case x: Path[_, _, _] => x.get match {
          case Success(s) => dp.summary(s);
          case Failure(e) => e.getClass.getSimpleName + "/" + e.getMessage
        }
        case x => Strings.oneLine(x)
      }.map { case (f, Success(v)) => f.getName + " -> " + v }.mkString(getClass.getSimpleName + "(", ",", ")")
  }

  override def detailed(implicit dp: DisplayProcessor): String = {
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
