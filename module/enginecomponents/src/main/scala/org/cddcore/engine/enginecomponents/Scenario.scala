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

  override def toString = s"Scenario($situation produces $assertion $reason)/$definedInSourceCodeAt"
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

  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)
                                                     (because: c.Expr[PartialFunction[P, R]]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[PartialFunction[P, R]](because.splice, c.literal(show(because.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      becauseHolder(scenarioBuilder, ch)
    }
  }

  protected def becauseHolder[P, R](scenarioBuilder: ScenarioBuilder[P, R], because: CodeHolder[PartialFunction[P, R]]): Scenario[P, R] = {
    val scenario = scenarioBuilder.scenario
    val result = scenario.copy(reason = BecauseReason[P,R](because))
    scenarioBuilder.scl.modified(scenario, result)
    result.validate
    result
  }
}


case class ScenarioBuilder[P, R](scenario: Scenario[P, R])(implicit val scl: ChildLifeCycle[Scenario[P, R]]) {
  def because(because: PartialFunction[P, R]): Scenario[P, R] = macro ScenarioBuilder.becauseImpl[P, R]

  def when(when: P => Boolean)(implicit scl: ChildLifeCycle[Scenario[P, R]]) = {
    val expected = scenario.expectedOption match {
      case Some(expected) => expected
      case _ => throw new CalculatorNotGivenException(scenario.definedInSourceCodeAt)
    }
    val result = scenario.copy(reason = WhenReason[P, R](when, expected))
    scl.modified(scenario, result)
    result.validate
    result
  }

  def where(where: R => Boolean) = {
    val result = scenario.copy(assertion = ResultAssertion[P, R](where))
    scl.modified(scenario, result)
    result
  }

  def by(fn: P => R) = {
    val result = scenario.reason match {
      case SimpleReason(result: R) => scenario.copy(reason = SimpleReasonWithBy(fn))
      case WhenReason(when, _) => scenario.copy(reason = WhenByReason(when, fn))
    }
    scl.modified(scenario, result)
    result
  }


}

