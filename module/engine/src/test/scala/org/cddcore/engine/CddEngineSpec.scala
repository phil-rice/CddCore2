package org.cddcore.engine

import org.cddcore.enginecomponents.{EngineComponent, Scenario}
import org.cddcore.utilities.{CddSpec, ChildLifeCycle}


trait CddEngineSpec extends CddSpec {

  def mapErrorsToClassName[K](errors: Map[K, Exception]) = errors.map { case (k, e) => (k, e.getClass.getSimpleName) }

  def toErrors[P, R](block: ChildLifeCycle[EngineComponent[P, R]] => Scenario[P, R]) = {
    var remembered: Option[Scenario[P, R]] = None
    val e = new Engine[P, R] {
      remembered = Some(block(childLifeCycle))
    }
    (e, remembered.get, mapErrorsToClassName(e.hierarchyBuilder.holder.errors))
  }
}

trait CddNonRecursiveSpec[P, R] extends CddEngineSpec {
  def mockEngine[P, R]: P => R = _ => throw new RuntimeException("Should not try anything recursive in this test")
}
