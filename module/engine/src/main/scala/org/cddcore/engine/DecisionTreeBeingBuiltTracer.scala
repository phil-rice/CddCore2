package org.cddcore.engine

import org.cddcore.enginecomponents.{DefinedInSourceCodeAt, EngineComponent, Reference, UseCase}
import org.cddcore.utilities._

object DecisionTreeBeingBuiltTracer {

  def apply[P, R](e: AbstractEngine[P, R]) = {
    val scenarios = e.allScenarios.toList
    (1 to scenarios.size).toList.map(EngineForTracer(e, _))
  }

}

object EngineForTracer {
  def apply[P, R](oldEngine: AbstractEngine[P, R], numScenarios: Int) = {
    val scenarios = oldEngine.allScenarios.toList.take(numScenarios)
    val thisScenario = scenarios.last
    val definedInSourceCodeAt = thisScenario.definedInSourceCodeAt
    implicit val dp = oldEngine.dp
    new Engine[P, R](oldEngine.title +s"_scenario$numScenarios", oldEngine.references, definedInSourceCodeAt) {
      scenarios.foreach(s => childLifeCycle.created(s))
    }
  }
}


//abstract class AbstractEngine[P, R](initialTitle: String = "Untitled", val references: List[Reference] = List(), val definedInSourceCodeAt: DefinedInSourceCodeAt = DefinedInSourceCodeAt.definedInSourceCodeAt())
//                                   (implicit val hierarchy: Hierarchy[UseCase[P, R], EngineComponent[P, R]], dp: DisplayProcessor)
//  extends EngineComponent[P, R] with MutableHierarchyBuilderWithChildLifeCycle[UseCase[P, R], EngineComponent[P, R]] with ToSummary {
