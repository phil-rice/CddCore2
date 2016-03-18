package org.cddcore.utilities

object ChildLifeCycle {
  //  implicit def defaultLifeCycle[C] = new NullLifeCycle[C]
}

trait ChildLifeCycle[C] {
  def created(child: C)

  /** The intention here is that any exceptions that occur calculating the new child are dealt with */
  def update[X <: C](block: => X): X

  def childHasException(c: C, exception: Exception)


}

class NullLifeCycle[C] extends ChildLifeCycle[C] {
  def created(child: C) {}

  def update[X <: C](block: => X) = block

  def childHasException(c: C, exception: Exception) {}

}
