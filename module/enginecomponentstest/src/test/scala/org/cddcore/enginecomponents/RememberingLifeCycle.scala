package org.cddcore.enginecomponents

import org.cddcore.utilities.ChildLifeCycle

class RememberingLifeCycle[P, R] extends ChildLifeCycle[EngineComponent[P, R]] {
  type EC = EngineComponent[P, R]
  type S = Scenario[P, R]
  var created = List[EC]()
  var modified = List[String]()
  var errors = List[String]()
  var last: EC = null

  def asString(s: EC) = s match {
    case s: Scenario[P, R] => s.situation + "/" + s.assertion + "/" + s.reason.getClass.getSimpleName
  }

  def created(child: EC) = created = {
    last = child;
    created :+ child
  }

  def update[X <: EngineComponent[P, R]](newChild: => X): X = {
    try {
      val c = newChild
      modified = modified :+ asString(c)
      last = c
      c
    } catch {
      case e: Exception => childHasException(last, e); last.asInstanceOf[X]
    }
  }

  def childHasException(c: EngineComponent[P, R], exception: Exception) = errors = errors :+ exception.getClass.getSimpleName + "/" + exception.getMessage
}