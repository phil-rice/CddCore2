package org.cddcore.core.builder

import org.cddcore.core.engine.{EngineComponent, UseCase}
import org.cddcore.core.utilities.Lens


trait Hierarchy[H <: C, C] {
  def withNewChild(h: H, child: C): H

  def childToHolder(child: C): H

  def currentChild(h: H): C

  def modChild(h: H, fn: C => C): H
}


object Hierarchy {
  implicit def useCaseHierarcy[P, R] =
    new Hierarchy[UseCase[P, R], EngineComponent[P, R]] {
      def withNewChild(h: UseCase[P, R], child: EngineComponent[P, R]): UseCase[P, R] =
        h.copy(components = child :: h.components)

      def modChild(h: UseCase[P, R], fn: (EngineComponent[P, R]) => EngineComponent[P, R]) = h.components match {
        case oldHead :: tail => h.copy(components = fn(oldHead) :: tail)
      }

      def currentChild(h: UseCase[P, R]): EngineComponent[P, R] = h.components.head

      def childToHolder(child: EngineComponent[P, R]): UseCase[P, R] = child.asInstanceOf[UseCase[P, R]]
    }
}

case class HierarchyBuilder[H <: C, C](holder: H, depth: Int = 0)(implicit hierachy: Hierarchy[H, C]) {
  type B = HierarchyBuilder[H, C]

  import hierachy._

  def addChild(c: C): B = depthToHolderL.transform(this, withNewChild(_, c))

  def addNewParent(c: C): B = addChild(c).copy(depth = depth + 1)

  def popParent: B = copy(depth = depth - 1)

  def modCurrentChild(fn: C => C): B = depthToChildL.transform(this, fn)

  protected def thisToHolderL = Lens[B, H](_.holder, (ucb, uc) => copy(holder = uc))

  protected def depthToHolderL: Lens[B, H] = (1 to depth).foldLeft(thisToHolderL)((lens, i) => lens andThen holderToHeadAsHolderLens)

  protected def depthToChildL: Lens[B, C] = depthToHolderL.andThen(holderToChildLens)

  protected def holderToChildLens = Lens[H, C](ch => currentChild(ch), (ech: H, head: C) => modChild(ech, _ => head))

  protected def holderToHeadAsHolderLens = Lens[H, H](ch => childToHolder(currentChild(ch)), (ech, head) => modChild(ech, _ => head))
}