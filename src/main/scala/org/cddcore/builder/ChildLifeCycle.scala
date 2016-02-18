package org.cddcore.builder

trait ChildLifeCycle[C] {
  def created(child: C)

  def modified(oldChild: C, newChild: C)
}

class NullLifeCycle[C] extends ChildLifeCycle[C] {
  def created(child: C) {}

  def modified(oldChild: C, newChild: C) {}
}
