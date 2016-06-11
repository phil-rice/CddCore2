/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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

