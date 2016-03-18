package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.ChildLifeCycle

class RememberingLifeCycle[P, R] extends ChildLifeCycle[EngineComponent[P, R]] {
  type EC = EngineComponent[P, R]
  type S = Scenario[P, R]
  var created = List[EC]()
  var modified = List[String]()

  def asString(s: EC) = s match {
    case s: Scenario[P, R] => s.situation + "/" + s.assertion + "/" + s.reason.getClass.getSimpleName
  }

  def created(child: EC) = created = created :+ child

  def update[X <: EngineComponent[P, R]](newChild: => X): X = {
    val c = newChild
    modified = modified :+ asString(c)
    c
  }

  def wentWrong[E <: Throwable](c: EngineComponent[P, R], e: E): Unit = ???
}