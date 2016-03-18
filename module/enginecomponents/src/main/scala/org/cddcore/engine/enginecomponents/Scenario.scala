package org.cddcore.engine.enginecomponents

import org.cddcore.utilities.{ChildLifeCycle, CodeHolder, DisplayProcessor, ToSummary}

import scala.language.implicitConversions
import scala.reflect.macros._

class SomethingMarker[R]

trait DefinedInSourceCodeAt {
  def definedInSourceCodeAt: String
}

object Scenario {

  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)

  implicit def scenarioToScenarioBuilder[P, R](s: Scenario[P, R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = ScenarioBuilder[P, R](s)

  def something[R] = new SomethingMarker[R]

}

case class Scenario[P, R](situation: P, reason: ScenarioReason[P, R], assertion: ScenarioAssertion[P, R], definedInSourceCodeAt: String, title: String, comment: Option[String]) extends EngineComponent[P, R] with DefinedInSourceCodeAt with ToSummary with HasComment {
  def allScenarios = Seq(this)

  def apply(engine: P => R, p: P) = reason(engine, p)

  def isDefinedAt(engine: P => R, p: P) = reason.isDefinedAt(engine, p)

  def expectedOption = assertion match {
    case EqualsAssertion(expected) => Some(expected)
    case _ => None
  }


  def calcuateAssertionFor(engine: P => R, p: P) = {
    val result = reason.apply(engine, p)
    assertion.valid(p, result)
  }

  def validate = {
    reason match {
      case sr: ScenarioReasonThatIsntRecursive[P, R] =>
        if (!sr.isDefinedAt(situation)) throw new ReasonInvalidException(this)
        val actual = sr.apply(situation)
        if (assertion.valid(situation, actual) == false) throw AssertionInvalidException(this, actual)

      case _ => // can't check recursive things without the engine
    }
  }

  override def toString = s"Scenario($situation ${assertion.prettyDescription} ${reason.prettyDescription})/$definedInSourceCodeAt"

  override def toSummary(dp: DisplayProcessor): String = s"Scenario $definedInSourceCodeAt ${dp(situation)} ${dp(assertion)})"
}

case class FromSituationScenarioBuilder[P, R](situation: P) {

  private def producesPrim(definedAt: String, reason: ScenarioReason[P, R], assertion: ScenarioAssertion[P, R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
    val s = Scenario[P, R](situation, reason, assertion, definedAt, situation.toString, None)
    scl.created(s)
    s
  }

  def produces(result: R)(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
    val definedAt = EngineComponent.definedInSourceCodeAt()
    producesPrim(definedAt, SimpleReason(result, definedAt), EqualsAssertion(result))
  }

  def produces(s: SomethingMarker[R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
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
      changeReason(scenarioBuilder, _.withBecause(ch))
    }
  }

  def whenImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(when: c.Expr[P => Boolean]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[P => Boolean](when.splice, c.literal(show(when.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      changeReason(scenarioBuilder, _.withWhen(ch))
    }
  }

  def byImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(fn: c.Expr[P => R]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[P => R](fn.splice, c.literal(show(fn.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      changeReason(scenarioBuilder, _.withBy(ch))
    }
  }

  def byRecursionImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(fn: c.Expr[PartialFunction[(P => R, P), R]]): c.Expr[Scenario[P, R]] = {
    import c.universe._
    reify {
      val ch = CodeHolder[PartialFunction[(P => R, P), R]](fn.splice, c.literal(show(fn.tree)).splice)
      val scenarioBuilder = (c.Expr[ScenarioBuilder[P, R]](c.prefix.tree)).splice
      changeReason(scenarioBuilder, _.withByRecursion(ch))
    }
  }

  protected def changeScenario[P, R](scenarioBuilder: ScenarioBuilder[P, R], fn: Scenario[P, R] => Scenario[P, R]): Scenario[P, R] = {
    val scenario = scenarioBuilder.scenario
    scenarioBuilder.scl.update {
      val result = fn(scenario)
      result.validate
      result
    }
  }

  protected def changeReason[P, R](scenarioBuilder: ScenarioBuilder[P, R], fn: ScenarioReason[P, R] => ScenarioReason[P, R]): Scenario[P, R] =
    changeScenario(scenarioBuilder, s => s.copy(reason = fn(s.reason)))

}


case class ScenarioBuilder[P, R](scenario: Scenario[P, R])(implicit val scl: ChildLifeCycle[EngineComponent[P, R]]) {
  def because(because: PartialFunction[P, R]): Scenario[P, R] = macro ScenarioBuilder.becauseImpl[P, R]

  def when(when: P => Boolean): Scenario[P, R] = macro ScenarioBuilder.whenImpl[P, R]

  def where(where: R => Boolean) = scl.update(scenario.copy(assertion = ResultAssertion[P, R](where)))

  def by(fn: P => R): Scenario[P, R] = macro ScenarioBuilder.byImpl[P, R]

  def byRecursion(fn: PartialFunction[(P => R, P), R]) = macro ScenarioBuilder.byRecursionImpl[P, R]

  def title(title: String) = ScenarioBuilder.changeScenario[P, R](this, _.copy(title = title))

  def withComment(comment: String) = ScenarioBuilder.changeScenario[P, R](this, _.copy(comment = Some(comment)))
}

