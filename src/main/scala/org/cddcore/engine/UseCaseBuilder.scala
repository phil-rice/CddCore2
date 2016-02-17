package org.cddcore.engine

import org.cddcore.utilities.Lens

object UseCaseBuilder {
  def identity[X] = Lens[X,X](a => a, (a,b) => b)

  def toChildren[P, R] = Lens[UseCaseBuilder[P,R], List[EngineComponent[P,R]]](_.children, (ucb,cs) => ucb.copy(children=cs))

  def listToHead[X] = Lens[List[X], X](_.head, (l,i) => i :: l.tail)

  def ucbToHead[P, R] = toChildren[P, R].andThen(listToHead)

  def titleLens[P, R] = Lens[UseCaseBuilder[P,R], String](_.title, (ucb,t) => ucb.copy(title = t))
}

case class UseCaseBuilder[P, R](title: String, children: List[EngineComponent[P, R]], lensToCurrent: Lens[UseCaseBuilder[P, R], UseCaseBuilder[P, R]]) {

  import UseCaseBuilder._

  def addUseCaseBuilder(c: UseCaseBuilder[P, R]) = lensToCurrent(this, ucb => copy()

  def modCurrentChild(fn: EngineComponent[P, R] => EngineComponent[P, R]) = lensToCurrent.andThen(ucbToHead[P, R]).mod(this, fn)


}
