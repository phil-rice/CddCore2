package org.cddcore.utilities

object ChildLifeCycle {
  implicit def defaultLifeCycle[C] = new NullLifeCycle[C]
}

trait ChildLifeCycle[C] {
  def created(child: C)

  def modify( newChild: C)

  def wentWrong[E <: Throwable](c: C, e: E)
}

class NullLifeCycle[C] extends ChildLifeCycle[C] {
  def created(child: C) {}

  def modify( newChild: C) {}

  def wentWrong[E <: Throwable](c: C, e: E) {}
}
