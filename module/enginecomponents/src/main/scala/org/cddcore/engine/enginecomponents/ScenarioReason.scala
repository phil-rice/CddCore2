package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.CodeHolder


sealed trait ScenarioReason[P, R] extends PartialFunction[P, R] with DefinedInSourceCodeAt {
  def prettyDescription: String

  def hasWhy: Boolean

  def withBecause(because: CodeHolder[PartialFunction[P, R]]): ScenarioReason[P, R]

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R]

  def withWhen(when: CodeHolder[P => Boolean]): ScenarioReason[P, R]
}

case class NotYetValid[P, R](definedInSourceCodeAt: String) extends ScenarioReason[P, R] {
  def hasWhy = false

  def apply(p: P) = throw new CalculatorNotGivenException(definedInSourceCodeAt)

  def isDefinedAt(p: P) = true

  def prettyDescription: String = "NotYetValid"

  def withBecause(because: CodeHolder[PartialFunction[P, R]]): ScenarioReason[P, R] = ???

  def withBy(by: CodeHolder[(P) => R]): ScenarioReason[P, R] = ???

  def withWhen(when: CodeHolder[(P) => Boolean]): ScenarioReason[P, R] = ???

}

case class SimpleReason[P, R](result: R, definedInSourceCodeAt: String) extends ScenarioReason[P, R] {
  def apply(p: P) = result

  def isDefinedAt(x: P): Boolean = true

  def hasWhy = false

  override def toString = s"SimpleReason($result)"

  def prettyDescription: String = "JustBecause"

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = new BecauseReason(because, definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = new WhenByReason(None, Left(by), definedInSourceCodeAt)

  def withWhen(when: CodeHolder[P => Boolean]) = new WhenByReason(Some(when), Right(result), definedInSourceCodeAt)
}

case class WhenByReason[P, R](when: Option[CodeHolder[P => Boolean]], byOrResult: Either[CodeHolder[P => R], R], definedInSourceCodeAt: String) extends ScenarioReason[P, R] {
  def prettyDescription: String = {
    val whenOpt = when.map("when " + _.prettyDescription)
    val byOpt: Option[String] = byOrResult.fold(by => Some("by " + by.prettyDescription), _ => None)
    List(whenOpt, byOpt).flatten.mkString(" ")
  } 

  def hasWhy: Boolean = when.isDefined

  def apply(p: P) = byOrResult.fold(by => by.fn(p), r => r)

  def isDefinedAt(x: P): Boolean = when.map(_.fn(x)).getOrElse(true)

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = byOrResult match {
    case Left(oldBy) => throw new ScenarioCannotHaveSecondByException(definedInSourceCodeAt)
    case Right(_) => copy(byOrResult = Left(by))
  }

  def withWhen(newWhen: CodeHolder[P => Boolean]) = when match {
    case None => copy(when = Some(newWhen))
    case Some(oldWhen) => throw new ScenarioCannotHaveSecondWhenException(definedInSourceCodeAt)
  }

}

case class BecauseReason[P, R](because: CodeHolder[PartialFunction[P, R]], definedInSourceCodeAt: String) extends ScenarioReason[P, R] {
  def prettyDescription: String = "because " + because.prettyDescription

  def hasWhy: Boolean = true

  def isDefinedAt(x: P): Boolean = because.fn.isDefinedAt(x)

  def apply(p: P) = because.fn(p)

  def withBecause(because: CodeHolder[PartialFunction[P, R]]) = throw new ScenarioCannotHaveSecondBecauseException(definedInSourceCodeAt)

  def withBy(by: CodeHolder[P => R]): ScenarioReason[P, R] = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

  def withWhen(when: CodeHolder[P => Boolean]): ScenarioReason[P, R] = throw new ScenarioCannotHaveWhenByAndBecauseException(definedInSourceCodeAt)

}

//}
//case class SimpleReasonWithBy[P, R](fn: CodeHolder[P => R]) extends ScenarioReason[P, R] {
//  def apply(p: P) = fn.fn(p)
//
//  def isDefinedAt(x: P): Boolean = true
//
//  def hasWhy = false
//
//  override def toString = s"SimpleReasonWithBy()"
//
//  def prettyDescription: String = toString
//
//}
//
//
//case class WhenReason[P, R](when: CodeHolder[P => Boolean], result: R) extends ScenarioReason[P, R] {
//  def apply(p: P) = result
//
//  def isDefinedAt(p: P): Boolean = when.fn(p)
//
//  def hasWhy = true
//
//  override def toString = s"when ${when.prettyDescription}"
//
//  def prettyDescription: String = toString
//
//}
//
//case class WhenByReason[P, R](when: CodeHolder[P => Boolean], fn: CodeHolder[P => R]) extends ScenarioReason[P, R] {
//  def apply(p: P) = fn.fn(p)
//
//  def isDefinedAt(p: P): Boolean = when.fn(p)
//
//  def hasWhy = true
//
//  override def toString = s"when ${when.prettyDescription} by ${fn.prettyDescription}"
//
//  def prettyDescription: String = toString
//}
//
//case class BecauseReason[P, R](pf: CodeHolder[PartialFunction[P, R]]) extends ScenarioReason[P, R] {
//  def apply(p: P) = pf.fn(p)
//
//  def isDefinedAt(p: P) = pf.fn.isDefinedAt(p)
//
//  def hasWhy = true
//
//  override def toString = s"because ${pf.prettyDescription}"
//
//  def prettyDescription: String = toString
//}