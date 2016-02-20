package org.cddcore.utilities


trait Hierarchy[H <: C, C] {
  def withNewChild(h: H, child: C): H

  def childToHolder(child: C): H

  def currentChild(h: H): Option[C]

  def modChild(h: H, fn: C => C): H
}


case class HierarchyBuilder[H <: C, C](holder: H, depth: Int = 0)(implicit hierachy: Hierarchy[H, C]) {
  type B = HierarchyBuilder[H, C]

  import hierachy._

  def addChild(c: C): B = depthToHolderL.transform(this, withNewChild(_, c))

  def addNewParent(c: C): B = addChild(c).copy(depth = depth + 1)

  def popParent: B = copy(depth = depth - 1)

  def modCurrentChild(fn: C => C): B = depthToChildL.transform(this, _ match {
    case Some(c) => Some(fn(c))
    case _ => throw new IllegalStateException("No current child")
  })

  def getCurrentChild = depthToChildL.get(this)


  protected def thisToHolderL = Lens[B, H](_.holder, (ucb, uc) => copy(holder = uc))

  protected def depthToHolderL: Lens[B, H] = (1 to depth).foldLeft(thisToHolderL)((lens, i) => lens andThen holderToHeadAsHolderLens)

  protected def depthToChildL: Lens[B, Option[C]] = depthToHolderL.andThen(holderToChildLens)

  protected def holderToChildLens = Lens[H, Option[C]](ch => currentChild(ch), (ech: H, head: Option[C]) =>
    head match {
      case Some(h) => modChild(ech, _ => h)
      case _ => throw new IllegalStateException("No current child")
    })

  protected def holderToHeadAsHolderLens = Lens[H, H](ch => currentChild(ch) match {
    case Some(c) => childToHolder(c)
    case _ => throw new IllegalStateException("No current child")
  }, (ech, head) => modChild(ech, _ => head))
}