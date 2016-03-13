package org.cddcore.rendering

import org.json4s.DefaultFormats
import org.json4s.jackson.{Serialization, JsonMethods}

object JsonForRendering {

  def pretty(map: Map[String, Any]) = {
    implicit val formats = DefaultFormats
    Serialization.writePretty(map)

  }
}
