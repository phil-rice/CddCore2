package org.cddcore.examples

import org.cddcore.engine.Engine

case class TrafficLight(red: Boolean = false, orange: Boolean = false, green: Boolean = false)

object TrafficLightEngine {
  //This isn't complex enough to really need use cases
  val decide = new Engine[TrafficLight, String]("Traffic Lights") {
    useCase("Cars need to obey traffic signals") {
      TrafficLight(red = true) produces "Stop"
      TrafficLight(red = true, orange = true) produces "Stop"
      TrafficLight(green = true) produces "Go" when (_.green)
      TrafficLight(orange = true) produces "Stop"
    }
  }

}
