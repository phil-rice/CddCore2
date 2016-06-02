package org.cddcore.json

import org.cddcore.structure.{PathRootAndSteps, Situation, Structure}
import org.json4s.jackson.{JsonMethods, Serialization}
import org.json4s.{DefaultFormats, JValue}

/** To use this you probably need to
import JsonSituation._
  */

class JsonSituation (implicit structure: Structure[JValue]) extends Situation[JValue] {
  def parse(s: String)(implicit formats: DefaultFormats = DefaultFormats) = root(JsonMethods.parse(s))


}

object JsonSituation {

  implicit object JsonStructure extends Structure[JValue] {

    override def findResult(pathRootAndSteps: PathRootAndSteps[JValue]): Iterable[JValue] = {
      import pathRootAndSteps._
      List(steps.foldLeft(root: JValue) { case (acc, step) =>
        step.linked match {
          case true => acc \ step.element
          case false => acc \\ step.element
        }
      })
    }

    override def structureTitle: String = "json"

    override def sToString(s: JValue): String = {
      implicit val formats = DefaultFormats
      Serialization.writePretty(s)
    }
  }

}

