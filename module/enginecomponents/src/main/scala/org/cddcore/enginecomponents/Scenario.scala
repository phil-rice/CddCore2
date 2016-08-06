/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.{ChildLifeCycle, CodeHolder, DisplayProcessor, ToSummary}

import scala.language.implicitConversions
import scala.reflect.macros._

class SomethingMarker[R]


object Scenario {

  implicit def pToScenarioBuilder[P, R](p: P) = FromSituationScenarioBuilder[P, R](p)

  implicit def scenarioToScenarioBuilder[P, R](s: Scenario[P, R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = ScenarioBuilder[P, R](s)

  def something[R] = new SomethingMarker[R]

}

case class Scenario[P, R](situation: P, reason: ScenarioReason[P, R], assertions: List[ScenarioAssertion[P, R]], definedInSourceCodeAt: DefinedInSourceCodeAt, title: String, comment: Option[String], references: List[Reference], canMerge: Boolean) extends EngineComponent[P, R] with HasComment with ToSummary {
  def allScenarios = Seq(this)

  def assertionsAllValid(p: P, r: R) = assertions.forall(_.valid(p, r))

  def apply(engine: P => R, p: P) = reason(engine, p)

  def isDefinedAt(engine: P => R, p: P) = reason.isDefinedAt(engine, p)

  def expectedOption = assertions.collect{    case EqualsAssertion(expected) => expected}.headOption

  def calcuateAssertionFor(engine: P => R, p: P) = {
    val result = reason.apply(engine, p)
    assertions.forall(_.valid(p, result))
  }

  def validate = {
    reason match {
      case sr: ScenarioReasonThatIsntRecursive[P, R] =>
        if (!sr.isDefinedAt(situation)) throw new ReasonInvalidException(this)
        val actual = sr.apply(situation)
        assertions.foreach(assertion =>        if (assertion.valid(situation, actual) == false) throw AssertionInvalidException(this, assertion, actual))

      case _ => // can't check recursive things without the engine
    }
  }

  override def toString = s"Scenario($situation ${assertions.map(_.prettyDescription).mkString(" and ")} ${reason.prettyDescription})/$definedInSourceCodeAt"

  override def summary(implicit dp: DisplayProcessor): String = comment.fold(s"${dp.summary(situation)} ${assertions.map(dp.summary).mkString(" and ")})")(c => s"$c")
}

case class FromSituationScenarioBuilder[P, R](situation: P) {

  private def producesPrim(definedAt: DefinedInSourceCodeAt, reason: ScenarioReason[P, R], assertion: ScenarioAssertion[P, R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
    val s = Scenario[P, R](situation, reason, List(assertion), definedAt, situation.toString, None, List(), false)
    scl.created(s)
    s
  }

  def produces(result: R)(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
    val definedAt = DefinedInSourceCodeAt.definedInSourceCodeAt()
    producesPrim(definedAt, SimpleReason(result, definedAt), EqualsAssertion(result))
  }

  def produces(s: SomethingMarker[R])(implicit scl: ChildLifeCycle[EngineComponent[P, R]]) = {
    val definedAt = DefinedInSourceCodeAt.definedInSourceCodeAt()
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

  protected def changeReason[P, R](scenarioBuilder: ScenarioBuilder[P, R], fn: BuildableScenarioReason[P, R] => BuildableScenarioReason[P, R]): Scenario[P, R] =
    changeScenario(scenarioBuilder, s => s.reason match { case  b: BuildableScenarioReason[P,R] => s.copy(reason = fn(b))})

}


case class ScenarioBuilder[P, R](scenario: Scenario[P, R])(implicit val scl: ChildLifeCycle[EngineComponent[P, R]]) {
  def because(because: PartialFunction[P, R]): Scenario[P, R] = macro ScenarioBuilder.becauseImpl[P, R]

  def when(when: P => Boolean): Scenario[P, R] = macro ScenarioBuilder.whenImpl[P, R]

  def where(where: R => Boolean) = scl.update(scenario.copy(assertions = List(ResultAssertion[P, R](where))))

  def by(fn: P => R): Scenario[P, R] = macro ScenarioBuilder.byImpl[P, R]

  def byRecursion(fn: PartialFunction[(P => R, P), R]) = macro ScenarioBuilder.byRecursionImpl[P, R]

  def title(title: String) = ScenarioBuilder.changeScenario[P, R](this, _.copy(title = title))

  def withComment(comment: String) = ScenarioBuilder.changeScenario[P, R](this, _.copy(comment = Some(comment)))

  def ref(document: Document) = ScenarioBuilder.changeScenario[P, R](this, s => s.copy(references = s.references :+ Reference(document, None)))

  def ref(documentAndInternalRef: (Document, String)) = ScenarioBuilder.changeScenario[P, R](this, s => s.copy(references = Reference(documentAndInternalRef._1, Some(documentAndInternalRef._2)) :: s.references))

  def allows(flag: Allows) = flag match {
    case merge => ScenarioBuilder.changeScenario[P, R](this, s => s.copy(canMerge = true))
  }
}

