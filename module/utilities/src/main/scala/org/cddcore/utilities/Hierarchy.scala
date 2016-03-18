package org.cddcore.utilities

trait Hierarchy[H <: C, C] {
  def withNewChild(h: H, child: C): H

  def childToHolder(child: C): H

  def lastAddedChild(h: H): Option[C]

  def modChild(h: H, fn: C => C): H

  /** WARNING returns new top parent */
  def badChild(topParent: H, child: C, exception: Exception): H
}

case class HierarchyBuilder[H <: C, C](holder: H, depth: Int = 0)(implicit hierarchy: Hierarchy[H, C]) {
  type B = HierarchyBuilder[H, C]

  import hierarchy._

  def addChild(c: C): B = depthToHolderL.transform(this, withNewChild(_, c))

  def addNewParent(c: C): B = addChild(c).copy(depth = depth + 1)

  def popParent: B = copy(depth = depth - 1)

  def modCurrentChild(fn: C => C): B = try {
    depthToChildL.transform(this, _ match {
      case Some(c) => Some(fn(c))
      case _ => throw new IllegalStateException("No current child")
    })
  } catch {
    case e: Exception => childHasException(currentChild.getOrElse(throw new RuntimeException("Somehow throw an exception modifying a child when there wasn't a child", e)), e)
  }

  def childHasException(c: C, exception: Exception) = thisToHolderL.transform(this, _ => badChild(holder, c, exception))

  def currentChild: Option[C] = depthToChildL.get(this)

  def currentParent: H = depthToHolderL.get(this)

  protected def thisToHolderL = Lens[B, H](_.holder, (ucb, uc) => copy(holder = uc))

  protected def depthToHolderL: Lens[B, H] = (1 to depth).foldLeft(thisToHolderL)((lens, i) => lens andThen holderToHeadAsHolderLens)

  protected def depthToChildL: Lens[B, Option[C]] = depthToHolderL.andThen(holderToChildLens)

  protected def holderToChildLens = Lens[H, Option[C]](ch => lastAddedChild(ch), (ech: H, head: Option[C]) =>
    head match {
      case Some(h) => modChild(ech, _ => h)
      case _ => throw new IllegalStateException("No current child")
    })

  protected def holderToHeadAsHolderLens = Lens[H, H](ch => lastAddedChild(ch) match {
    case Some(c) => childToHolder(c)
    case _ => throw new IllegalStateException("No current child")
  }, (ech, head) => modChild(ech, _ => head))
}


/** An example of this is the 'Engine' where scenarios and use cases are built using mutable state, because the DSL reads better
  *
  * The methods are protected so that they are only availble from within a class extending this. They can of course be exposed: see SimpleMutableHierarchyBuilder */
trait MutableHierarchyBuilder[H <: C, C] {
  implicit protected def hierarchy: Hierarchy[H, C]

  var hierarchyBuilder = new HierarchyBuilder[H, C](makeRootHolder, 0)

  protected def seal = ???

  protected def makeRootHolder: H

  protected def modBuilder(fn: HierarchyBuilder[H, C] => HierarchyBuilder[H, C]) = hierarchyBuilder = fn(hierarchyBuilder)

  protected def addChild(c: C) = modBuilder(_.addChild(c))

  protected def addNewParent(c: C) = modBuilder(_.addNewParent(c))

  protected def popParent = modBuilder(_.popParent)

  protected def modCurrentChild(fn: C => C) = modBuilder(_.modCurrentChild(fn))

  protected def getCurrentChild = hierarchyBuilder.currentChild

  protected def childHasException(c: C, exception: Exception) = modBuilder(_.childHasException(c, exception))
}

class SimpleMutableHierarchyBuilder[H <: C, C](val makeRootHolder: H)(implicit val hierarchy: Hierarchy[H, C]) extends MutableHierarchyBuilder[H, C] {

  override def addChild(c: C) = super.addChild(c)

  override def addNewParent(c: C) = super.addNewParent(c)

  override def popParent = super.popParent

  override def modCurrentChild(fn: C => C) = super.modCurrentChild(fn)

  override def getCurrentChild = super.getCurrentChild

  override def childHasException(c: C, exception: Exception) = super.childHasException(c, exception)

  override def seal = super.seal

}

trait MutableHierarchyBuilderWithChildLifeCycle[H <: C, C] extends MutableHierarchyBuilder[H, C] {

  protected def addParentChildrenDefinedInBlock(parent: => H)(block: => Unit) = {
    addNewParent(parent)
    block
    popParent
  }

  implicit val childLifeCycle = new ChildLifeCycle[C] {
    def created(child: C): Unit = addChild(child)


    def update[X <: C](newChild: => X): X = {
      modCurrentChild(_ => newChild)
      getCurrentChild.getOrElse(throw new RuntimeException("Somehow don't have a current child after having modified it")).asInstanceOf[X]
    }

    def wentWrong[E <: Throwable](c: C, e: E): Unit = ???
  }
}
