package org.cddcore.enginecomponents

import org.cddcore.utilities.CodeHolder


sealed trait ScenarioReason[P, R] extends HasDefinedInSourceCodeAt {
  def prettyDescription: String

  def hasWhy: Boolean

  def withBecause(because: CodeHolder[PartialFunction[P, R]]): ScenarioReason[P, R]

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R]

  def withWhen(when: CodeHolder[P => Boolean]): ScenarioReason[P, R]

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]): ScenarioReason[P, R]

  def isDefinedAt(engine: P => R, p: P): Boolean

  def apply(engine: P => R, p: P): R
}

trait ScenarioReasonThatIsntRecursive[P, R] extends ScenarioReason[P, R] {
  def isDefinedAt(p: P): Boolean

  def apply(p: P): R

  def apply(engine: P => R, p: P): R = apply(p)

  def isDefinedAt(engine: P => R, p: P): Boolean = isDefinedAt(p)


}

case class NotYetValid[P, R](definedInSourceCodeAt: DefinedInSourceCodeAt) extends ScenarioReason[P, R] {
  def hasWhy = false


  def prettyDescription: String = "NotYetValid"

  def withBecause(because: CodeHolder[PartialFunction[P, R]]): ScenarioReason[P, R] = ???

  def withBy(by: CodeHolder[(P) => R]): ScenarioReason[P, R] = ???

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]): ScenarioReason[P, R] = ???

  def withWhen(when: CodeHolder[(P) => Boolean]): ScenarioReason[P, R] = ???

  def apply(engine: P => R, p: P): R = throw new CalculatorNotGivenException(definedInSourceCodeAt)

  def isDefinedAt(engine: P => R, p: P) = true
}

case class SimpleReason[P, R](result: R, definedInSourceCodeAt: DefinedInSourceCodeAt) extends ScenarioReasonThatIsntRecursive[P, R] {
  def apply(p: P) = result

  def isDefinedAt(p: P): Boolean = true

  def hasWhy = false

  override def toString = s"SimpleReason($result)"

  def prettyDescription: String = "JustBecause"

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = new BecauseReason(because, definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = new WhenByReason(None, Left(by), definedInSourceCodeAt)

  def withWhen(when: CodeHolder[P => Boolean]) = new WhenByReason(Some(when), Right(result), definedInSourceCodeAt)

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]): ScenarioReason[P, R] = new ByRecursionReason(by, definedInSourceCodeAt)
}

case class WhenByReason[P, R](when: Option[CodeHolder[P => Boolean]], byOrResult: Either[CodeHolder[P => R], R], definedInSourceCodeAt: DefinedInSourceCodeAt) extends ScenarioReasonThatIsntRecursive[P, R] {
  def prettyDescription: String = {
    val whenOpt = when.map("when " + _.prettyDescription)
    val byOpt: Option[String] = byOrResult.fold(by => Some("by " + by.prettyDescription), _ => None)
    List(whenOpt, byOpt).flatten.mkString(" ")
  }

  def hasWhy: Boolean = when.isDefined

  def apply(p: P) = byOrResult.fold(by => by.fn(p), r => r)

  def isDefinedAt(p: P): Boolean = when.map(_.fn(p)).getOrElse(true)

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = byOrResult match {
    case Left(oldBy) => throw new ScenarioCannotHaveSecondByException(definedInSourceCodeAt)
    case Right(_) => copy(byOrResult = Left(by))
  }

  def withWhen(newWhen: CodeHolder[P => Boolean]) = when match {
    case None => copy(when = Some(newWhen))
    case Some(oldWhen) => throw new ScenarioCannotHaveSecondWhenException(definedInSourceCodeAt)
  }

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]) = throw new ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt)
}

case class BecauseReason[P, R](because: CodeHolder[PartialFunction[P, R]], definedInSourceCodeAt: DefinedInSourceCodeAt) extends ScenarioReasonThatIsntRecursive[P, R] {
  def prettyDescription: String = "because " + because.prettyDescription

  def hasWhy: Boolean = true

  def isDefinedAt(p: P): Boolean = because.fn.isDefinedAt(p)

  def apply(p: P) = because.fn(p)

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = throw new ScenarioCannotHaveSecondBecauseException(definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

  def withWhen(when: CodeHolder[P => Boolean]): ScenarioReason[P, R] = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]) = throw new ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt)
}

case class ByRecursionReason[P, R](byRecursion: CodeHolder[PartialFunction[(P => R, P), R]], definedInSourceCodeAt: DefinedInSourceCodeAt) extends ScenarioReason[P, R] {
  def prettyDescription: String = "byRecursion " + byRecursion.prettyDescription

  def hasWhy: Boolean = true

  def isDefinedAt(engine: P => R, p: P): Boolean = byRecursion.fn.isDefinedAt(engine, p)

  def apply(engine: P => R, p: P) = byRecursion.fn(engine, p)

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = throw new ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = throw new ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt)

  def withWhen(when: CodeHolder[P => Boolean]): ScenarioReason[P, R] = throw new ScenarioCannotHaveByRecursonIfWhenByOrBecauseAlreadyDefinedException(definedInSourceCodeAt)

  def withByRecursion(by: CodeHolder[PartialFunction[(P => R, P), R]]) = throw new ScenarioCannotHaveSeconByRecursionException(definedInSourceCodeAt)
}

