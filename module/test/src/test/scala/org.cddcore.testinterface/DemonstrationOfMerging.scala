package org.cddcore.testinterface

import java.io.File

import org.cddcore.engine.Engine
import org.cddcore.rendering.{DecisionTreeTraceRendering, RenderConfiguration}
import org.cddcore.utilities.Monitor


object DemonstrationOfMerging {

  case class Person(name: String, hasGun: Boolean = false, hasDrugs: Boolean = false, hasLotsOfMoney: Boolean = false, isOnWatchList: Boolean = false, isFlagged: Boolean = false) {
    def hasIllegalItem = hasDrugs || hasGun
  }

  implicit val monitor = Monitor.printlnMonitor
  val engine = new Engine[Person, String] {
    useCase("People without anything wrong with them are ignored") {
      Person("Jane Doe") produces "ignore"
    }

    useCase("People with guns on them are arrested") {
      Person("Nasty Ned", hasDrugs = true, hasGun = true) produces "arrest" when (_.hasGun) allows merge
      Person("Shooter BSid", hasGun = true) produces "arrest"
    }

    useCase("People who have drugs on them are arrested") {
      Person("Don Don", hasGun = true, hasDrugs = true) produces "arrest" when (_.hasDrugs) allows merge
      Person("Junky Jill", hasDrugs = true) produces "arrest"
    }
  }

  implicit val renderConfiguration = RenderConfiguration.defaultRenderConfiguration.copy(referenceFilesUrlBase = "reference")
  println("MAKING THE TRACE")
  DecisionTreeTraceRendering.makeFile(new File("./target/cddTrace.html"), engine)
  engine.buildDecisionTree

}
