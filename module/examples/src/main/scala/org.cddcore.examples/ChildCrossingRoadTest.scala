/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.examples

import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.engine.Engine
import org.junit.runner.RunWith


@RunWith(classOf[CddRunner])
class ChildCrossingRoadTest extends CddContinuousIntegrationTest {
  val engines = List(ChildCrossingRoad.safeToCross2)
}

object ChildCrossingRoad {

  case class carPosition(position: String, direction: String, speed: Int, distance: Int) {
    val timeToArrive: Double = distance / (speed.toFloat + 0.0001)
    override val toString = s"car on my %s and travelling %s with a speed of %d metres per sec and %d metres away".format(position, direction, speed, distance)
  }

  val safeToCross = new Engine[carPosition, String]("safeToCross") {
    title("Is a car safe?")

    useCase("Basic Safety", "All cars are dangerous until proven otherwise") {
      carPosition("left", "right", 10, 5) produces "stop"
    }

    useCase("Parked Cars", "Vehicles at rest without visible signs of movement are safe") {
      carPosition("left", "right", 0, 5) produces "go" when { car => car.speed == 0 }
    }

    useCase("Travelling Away", "Cars facing away and moving away are safe") {
      carPosition("left", "left", 4, 4) produces "go" because { case car if car.position == car.direction => "go" }
      carPosition("right", "right", 0, 10) produces "go"
      carPosition("right", "right", 5, 8) produces "go"
    }

    useCase("Slow vehicles", "Cars that are so far away, or travelling so slowly that they cannot reach our crossing point within the time taken to cross are considered safe") {
      carPosition("left", "right", 1, 112) produces "go" when { car => car.timeToArrive > 8 }
    }
  }


  val safeToCross2 = new Engine[carPosition, Boolean]("safeToCross2") {
    title("Is a car safe?")

    useCase("Basic Safety", "All cars are dangerous until proven otherwise") {
      carPosition("left", "right", 10, 5) produces false
    }

    useCase("Parked Cars", "Vehicles at rest without visible signs of movement are safe") {
      carPosition("left", "right", 0, 5) produces true when { car => car.speed == 0 }
    }

    useCase("Travelling Away", "Cars facing away and moving away are safe") {
      carPosition("left", "left", 4, 4) produces true because { case car if car.position == car.direction => true}
      carPosition("right", "right", 0, 10) produces true
      carPosition("right", "right", 5, 8) produces true
    }

    useCase("Slow vehicles", "Cars that are so far away, or travelling so slowly that they cannot reach our crossing point within the time taken to cross are considered safe") {
      carPosition("left", "right", 1, 112) produces true when { car => car.timeToArrive > 8 }
    }
  }
}
