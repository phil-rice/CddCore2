package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.{CodeHolder, ChildLifeCycle}

import scala.language.implicitConversions
import scala.reflect.macros._

class SomethingMarker[R]

object Scenario {


  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)

  implicit def scenarioToScenarioBuilder[P, R](s: Scenario[P, R])(implicit scl: ChildLifeCycle[Scenario[P, R]]) = ScenarioBuilder[P, R](s)

  def something[R] = new SomethingMarker[R]

}

case class Scenario[P, R](situation: P, reason: ScenarioReason[P, R], assertion: ScenarioAssertion[P, R], definedInSourceCodeAt: String) extends EngineComponent[P, R] with PartialFunction[P, R] {
  def allScenarios = Seq(this)

  def isDefinedAt(p: P) = reason.isDefinedAt(p)

  def expectedOption = assertion match {
    case EqualsAssertion(expected) => Some(expected)
    case _ => None
  }

  def calcuateAssertionFor(p: P) = {
    val result = apply(p)
    assertion.valid(p, result)
  }

  def apply(p: P) = reason(p)

  def validate = {
    if (!isDefinedAt(situation)) throw new ReasonInvalidException(this)
    val actual = apply(situation)
    if (assertion.valid(situation, actual) == false) throw AssertionInvalidException(this, actual)
  }

  override def toString = s"Scenario($situation ${assertion.prettyDescription} ${reason.prettyDescription})/$definedInSourceCodeAt"
}

case class FromSituationScenarioBuilder[P, R](situation: P) {
  private def producesPrim(definedAt: String, reason: ScenarioReason[P, R], assertion: ScenarioAssertion[P, R])(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val s = Scenario[P, R](situation, reason, assertion, definedAt)
    scl.created(s)
    s
    //    new ScenarioBuilder[P,R](s)
  }

  def produces(result: R)(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val definedAt = EngineComponent.definedInSourceCodeAt()
    producesPrim(definedAt, SimpleReason(result), EqualsAssertion(result))
  }

  def produces(s: SomethingMarker[R])(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val definedAt = EngineComponent.definedInSourceCodeAt()
    producesPrim(definedAt, NotYetValid(definedAt), new UnknownAssertion)
  }


}

import scala.language.experimental.macros

object ScenarioBuilder {

  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(because: c.Expr[PartialFunction[P, R]]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[PartialFunction[P, R]](because.splice, c.literal(show(because.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      becauseHolder(scenarioBuilder, ch)
    }
  }

  def whenImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(when: c.Expr[P => Boolean]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[P => Boolean](when.splice, c.literal(show(when.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      whenHolder(scenarioBuilder, ch)
    }
  }

  def byImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(fn: c.Expr[P => R]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[P => R](fn.splice, c.literal(show(fn.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      byHolder(scenarioBuilder, ch)
    }
  }

  protected def becauseHolder[P, R](scenarioBuilder: ScenarioBuilder[P, R], because: CodeHolder[PartialFunction[P, R]]): Scenario[P, R] = {
    val scenario = scenarioBuilder.scenario
    scenario.reason match {
      case _: BecauseReason[_, _] => throw new ScenarioCannotHaveSecondBecauseException(scenario)
      case _: SimpleReasonWithBy[_, _] => throw new ScenarioCannotHaveByAndBecauseException(scenario)
      case _: WhenReason[_, _] => throw new ScenarioCannotHaveWhenAndBecauseException(scenario)
      case _: WhenByReason[_, _] => throw new ScenarioCannotHaveWhenAndBecauseException(scenario)
      case _: SimpleReason[_, _] => {
        val result = scenario.copy(reason = BecauseReason[P, R](because))
        scenarioBuilder.scl.modified(scenario, result)
        result.validate
        result
      }
      case _ => throw new MatchError(s"Don't know how to deal with ${scenario.reason.getClass.getSimpleName} in $scenario")
    }
  }

  protected def whenHolder[P, R](scenarioBuilder: ScenarioBuilder[P, R], when: CodeHolder[P => Boolean]) = {
    val scenario = scenarioBuilder.scenario
    scenario.reason match {
      case _: WhenReason[_, _] => throw new ScenarioCannotHaveSecondWhenException(scenario)
      case _: WhenByReason[_, _] => throw new ScenarioCannotHaveSecondWhenException(scenario)
      case _ => {
        val expected = scenario.expectedOption match {
          case Some(expected) => expected
          case _ => throw new CalculatorNotGivenException(scenario.definedInSourceCodeAt)
        }
        val result = scenario.copy(reason = WhenReason[P, R](when, expected))
        scenarioBuilder.scl.modified(scenario, result)
        result.validate
        result
      }
    }
  }

  protected def byHolder[P, R](scenarioBuilder: ScenarioBuilder[P, R], by: CodeHolder[P => R]) = {
    val scenario = scenarioBuilder.scenario
    val result = scenario.reason match {
      case _: BecauseReason[_, _] => throw new ScenarioCannotHaveByAndBecauseException(scenario)
      case SimpleReason(result: R) => scenario.copy(reason = SimpleReasonWithBy(by))
      case WhenReason(when, _) => scenario.copy(reason = WhenByReason(when, by))
      case WhenByReason(when, _) => throw new ScenarioCannotHaveSecondByException(scenario)
    }
    scenarioBuilder.scl.modified(scenario, result)
    result
  }

}


case class ScenarioBuilder[P, R](scenario: Scenario[P, R])(implicit val scl: ChildLifeCycle[Scenario[P, R]]) {
  def because(because: PartialFunction[P, R]): Scenario[P, R] = macro ScenarioBuilder.becauseImpl[P, R]

  def when(when: P => Boolean): Scenario[P, R] = macro ScenarioBuilder.whenImpl[P, R]

  def where(where: R => Boolean) = {
    val result = scenario.copy(assertion = ResultAssertion[P, R](where))
    scl.modified(scenario, result)
    result
  }

  def by(fn: P => R) = macro ScenarioBuilder.byImpl[P, R]


}

