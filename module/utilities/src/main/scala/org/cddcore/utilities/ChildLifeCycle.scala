package org.cddcore.utilities

object ChildLifeCycle {
//  implicit def defaultLifeCycle[C] = new NullLifeCycle[C]
}

trait ChildLifeCycle[C] {
  def created(child: C)

  /** The intention here is that any exceptions that occur calculating the new child are dealt with */
  def update[X <:C] (newChild: => X): X

  def wentWrong[E <: Throwable](c: C, e: E)
}

class NullLifeCycle[C] extends ChildLifeCycle[C] {
  def created(child: C) {}

  def update[X <:C] (newChild: => X) = newChild

  def wentWrong[E <: Throwable](c: C, e: E) {}
}
