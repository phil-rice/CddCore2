package org.cddcore.examples

import org.cddcore.website.WebServer
object Website {
  def main(args: Array[String]) {
    WebServer(List(Bowling.get)).launch//, Bowling.makeFrame, Factorial.factorial, Fibonacci.fibonacci, ProcessChequeXml.processCheque, Tennis.tennis, TrafficLightEngine.decide)).launch
  }


}
