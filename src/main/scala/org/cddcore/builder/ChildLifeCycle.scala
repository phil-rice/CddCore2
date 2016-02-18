package org.cddcore.builder

object ChildLifeCycle {
  implicit def defaultLifeCycle[C] = new NullLifeCycle[C]
}

trait ChildLifeCycle[C] {
  def created(child: C)

  def modified(oldChild: C, newChild: C)
}

class NullLifeCycle[C] extends ChildLifeCycle[C] {
  def created(child: C) {}

  def modified(oldChild: C, newChild: C) {}
}

