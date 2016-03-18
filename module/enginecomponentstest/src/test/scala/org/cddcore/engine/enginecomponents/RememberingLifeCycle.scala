package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.ChildLifeCycle

class RememberingLifeCycle[P, R] extends ChildLifeCycle[Scenario[P, R]] {
  type S = Scenario[P, R]
  var created = List[S]()
  var modified = List[String]()

  def asString(s: S) = s.situation + "/" + s.assertion + "/" + s.reason.getClass.getSimpleName

  def created(child: S) = created = created :+ child

  def modified(oldChild: S, newChild: S) = modified = modified :+ (asString(oldChild) + "==>" + asString(newChild))
}