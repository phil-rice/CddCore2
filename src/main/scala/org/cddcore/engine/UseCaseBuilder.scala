package org.cddcore.engine

import org.cddcore.utilities.Lens

case class UseCaseBuilder[P, R](useCase: UseCase[P, R], depth: Int = 0) {

  protected def depthToUseCase: Lens[UseCaseBuilder[P, R], UseCase[P, R]] = (1 to depth).foldLeft(usecaseBuilderToUseCase)((lens, i) => lens andThen useCaseToHeadUseCaseLens)

  protected def depthToUseCaseChild: Lens[UseCaseBuilder[P, R], EngineComponent[P, R]] = depthToUseCase.andThen(useCaseToHeadLens)

  def addChild(c: EngineComponent[P, R]) = depthToUseCase.transform(this, uc => uc.copy(components = c :: uc.components))

  def addNewParent(c: UseCase[P, R]): UseCaseBuilder[P, R] = addChild(c).copy(depth = depth + 1)

  def popParent: UseCaseBuilder[P,R] = copy(depth = depth - 1)

  def modCurrentChild(fn: EngineComponent[P, R] => EngineComponent[P, R]) = depthToUseCaseChild.transform(this, fn)

  protected def usecaseBuilderToUseCase = Lens[UseCaseBuilder[P, R], UseCase[P, R]](_.useCase, (ucb, uc) => ucb.copy(useCase = uc))

  protected def useCaseToHeadUseCaseLens = Lens[UseCase[P, R], UseCase[P, R]](
    ch => ch.components.head.asInstanceOf[UseCase[P, R]],
    (ech, head) => ech.components match {
      case h :: tail => ech.withComponents(head.asInstanceOf[EngineComponent[P, R]] :: tail)
    }
  )

  protected def useCaseToHeadLens = Lens[UseCase[P, R], EngineComponent[P, R]](
    ch => ch.components.head,
    (ech, head) => ech.components match {
      case h :: tail => ech.withComponents(head :: tail)
    }
  )

}
