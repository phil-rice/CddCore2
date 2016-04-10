/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
  * Another is the ThingDsl in ThingDslSpec
  *
  * The methods are protected so that they are only availble from within a class extending this. They can of course be exposed: see SimpleMutableHierarchyBuilder */
trait MutableHierarchyBuilder[H <: C, C] {
  implicit protected def hierarchy: Hierarchy[H, C]

  var hierarchyBuilder = new HierarchyBuilder[H, C](makeRootHolder, 0)
  private var hasBeenSealed = false

  def postSealMessage: String

  protected def seal = hasBeenSealed = true

  protected def makeRootHolder: H

  private def checkSeal[X](block: => X) = if (hasBeenSealed) throw new CannotAddItemsException(postSealMessage) else block

  protected def modBuilder(fn: HierarchyBuilder[H, C] => HierarchyBuilder[H, C]) =
    checkSeal(hierarchyBuilder = fn(hierarchyBuilder))

  protected def addChild(c: C) = modBuilder(_.addChild(c))

  protected def addNewParent(c: C) = modBuilder(_.addNewParent(c))

  protected def popParent = modBuilder(_.popParent)

  protected def modCurrentChild(fn: C => C) = modBuilder(_.modCurrentChild(fn))

  protected def getCurrentChild = checkSeal(hierarchyBuilder.currentChild)

  protected def childHasException(c: C, exception: Exception) = modBuilder(_.childHasException(c, exception))
}

class SimpleMutableHierarchyBuilder[H <: C, C](val makeRootHolder: H, val postSealMessage: String = "")(implicit val hierarchy: Hierarchy[H, C]) extends MutableHierarchyBuilder[H, C] {

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

    def update[X <: C](block: => X): X = {
      modCurrentChild(_ => block)
      getCurrentChild.getOrElse(throw new RuntimeException("Somehow don't have a current child after having modified it")).asInstanceOf[X]
    }

    def childHasException(c: C, exception: Exception) = MutableHierarchyBuilderWithChildLifeCycle.this.childHasException(c, exception)


  }
}
