package org.cddcore.rendering.view

import org.cddcore.engine.{ConflictingScenariosException, InvalidScenariosTestFramework}
import org.cddcore.enginecomponents.ReasonInvalidException
import org.cddcore.rendering.Renderer


class ExceptionViewSpec extends InvalidScenariosTestFramework {

  val runtimeException = new RuntimeException("someMessage")

  val view = new ExceptionView

  implicit val renderContext = Renderer.renderContext(invalidScenarios)
  val reason1Exception = exception[ReasonInvalidException[String, String]]("reason1")
  val bothHaveReasons2Exception = exception[ConflictingScenariosException[String, String]]("bothHaveReasons2")

  import View._

  "The Exception View apply method " should "produce a map with the class of exception in it" in {
    view(runtimeException).get(Class) shouldBe Some("RuntimeException")
    view(reason1Exception).get(Class) shouldBe Some("ReasonInvalidException")
  }

  it should "produce a map with the first five parts of the stack trace in it " in {
    view(runtimeException).get(Stack) shouldBe Some(runtimeException.getStackTrace.take(5).mkString("\n"))
    view(reason1Exception).get(Stack) shouldBe Some(reason1Exception.getStackTrace.take(5).mkString("\n"))
  }

  it should "produce a map with the message in it for exceptions that aren't scenario exceptions " in {
    view(runtimeException).get(Message) shouldBe Some("someMessage")
  }

  it should "produce a map with the message just being the main message for a Scenario Exception" in {
    view(reason1Exception).get(Message) shouldBe Some(reason1Exception.mainMessage)
  }

  it should "have an advice for HasAdvice exceptions and not for others" in {
    view(runtimeException).get(Advice) shouldBe None
    view(reason1Exception).get(Advice) shouldBe Some(reason1Exception.advice.mkString("<br />"))
  }

  it should "have an explaination for HasExplaination exceptions and not for others" in {
    view(runtimeException).get(Explaination) shouldBe None
    view(reason1Exception).get(Explaination) shouldBe None
    view(bothHaveReasons2Exception).get(Explaination) shouldBe Some(bothHaveReasons2Exception.explaination.mkString("<br />"))
  }

  it should "have an actual for HasActual exceptions and not for others" in {
    view(runtimeException).get(Actual) shouldBe None
    view(reason1Exception).get(Actual) shouldBe None
    view(bothHaveReasons2Exception).get(Actual) shouldBe Some(bothHaveReasons2Exception.actual)
  }

  it should "have a reason for HasReason exceptions and not for others" in {
    view(runtimeException).get(Reason) shouldBe None
    view(reason1Exception).get(Reason) shouldBe Some(reason1Exception.reason)
    view(bothHaveReasons2Exception).get(Reason) shouldBe None

  }
}
