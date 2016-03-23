package org.cddcore.cddunit

import org.cddcore.enginecomponents.{CannotAddScenarioException, ReasonInvalidException}
import org.cddcore.utilities.{Reflection, Hierarchy, MutableHierarchyBuilderWithChildLifeCycle, CddSpec}
import org.junit.runner.{Result, Description}
import org.junit.runner.notification.{Failure, RunListener, RunNotifier}

trait JunitNotification {
  override def toString = getClass.getSimpleName.dropRight(1)
}

object TestRunStarted extends JunitNotification

object TestRunFinished extends JunitNotification

object TestStarted extends JunitNotification

object TestFinished extends JunitNotification

case class TestFailed(clazz: Class[_ <: Throwable]) extends JunitNotification {
  override def toString = s"TestFailed(classOf[${clazz.getSimpleName}])"
}

trait NotifierData

case class NotifierEvent(id: String, notification: JunitNotification) extends NotifierData {
  override def toString = s"""NotifierEvent("$id", $notification)"""
}

case class NotifierHolder(events: Vector[NotifierData]) extends NotifierData {
  override def toString = s"NotifierHolder(Vector(\n   ${events.mkString(",\n   ")}))"
}

class NotifierDsl[P, R](descriptionDsl: DescriptionDsl[P, R]) extends MutableHierarchyBuilderWithChildLifeCycle[NotifierHolder, NotifierData] {
  implicit protected def hierarchy: Hierarchy[NotifierHolder, NotifierData] = new Hierarchy[NotifierHolder, NotifierData] {

    def badChild(topParent: NotifierHolder, child: NotifierData, exception: Exception): NotifierHolder = ???

    def lastAddedChild(h: NotifierHolder): Option[NotifierData] = h.events.lastOption

    def withNewChild(h: NotifierHolder, child: NotifierData): NotifierHolder = h.copy(events = h.events :+ child)

    def childToHolder(child: NotifierData): NotifierHolder = ???

    override def modChild(h: NotifierHolder, fn: (NotifierData) => NotifierData): NotifierHolder = {
      val withoutLast = h.events.dropRight(1)
      val newValue = withoutLast :+ fn(h.events.last)
      h.copy(events = newValue)
    }
  }

  override def postSealMessage: String = ???

  protected def makeRootHolder: NotifierHolder = new NotifierHolder(Vector())

  implicit val renderContext = descriptionDsl.renderContext

  def testRun(block: => Unit) = {
    addChild(NotifierEvent(descriptionDsl.description.uniqueId, TestRunStarted))
    block
    addChild(NotifierEvent(descriptionDsl.description.uniqueId, TestRunFinished))
  }

  def engine(block: => Unit) = {
    val id = renderContext.pathMap(descriptionDsl.engine)
    addChild(NotifierEvent(id, TestStarted))
    block
    addChild(NotifierEvent(id, TestFinished))

  }

  def testSuite(useCaseName: String)(block: => Unit): Unit = {
    val id = renderContext.pathMap(descriptionDsl.findUsecaseWithName(useCaseName))
    descriptionDsl.description.allChildrenIncludingMe.find(_.uniqueId == id) match {
      case Some(d) =>
        addChild(NotifierEvent(d.uniqueId, TestStarted))
        block
        addChild(NotifierEvent(d.uniqueId, TestFinished))
    }
  }

  def scenario(p: P) = {
    val id = renderContext.pathMap(descriptionDsl.findScenarioWithSituation(p))
    addChild(NotifierEvent(id, TestStarted))
    addChild(NotifierEvent(id, TestFinished))
  }

  def scenarioFailure[E <: Throwable](p: P, exceptionClass: Class[E]) = {
    val id = renderContext.pathMap(descriptionDsl.findScenarioWithSituation(p))
    addChild(NotifierEvent(id, TestStarted))
    addChild(NotifierEvent(id, TestFailed(exceptionClass)))
  }
}


class NotifierDslSpec extends CddSpec with DescriptionDslTestFramework {
  val notifierDsl = new NotifierDsl[Int, String](dsl) {
    testRun {
      engine {
        testSuite("a use case") {
          scenario(1)
          scenarioFailure(2, classOf[ReasonInvalidException[_, _]])
        }
        testSuite("another use case") {
          scenario(3)
          scenarioFailure(4, classOf[CannotAddScenarioException[_, _]])
          scenarioFailure(5, classOf[CannotAddScenarioException[_, _]])
        }
      }
    }
  }

  "The NotifierDsl" should "make easy to read lists of notifications" in {
    notifierDsl.hierarchyBuilder.holder shouldBe NotifierHolder(Vector(
      NotifierEvent("org.cddcore.cddunit.ExampleJUnit", TestRunStarted),
      NotifierEvent("An engine/index", TestStarted),
      NotifierEvent("An engine/1", TestStarted),
      NotifierEvent("An engine/1.1", TestStarted),
      NotifierEvent("An engine/1.1", TestFinished),
      NotifierEvent("An engine/1.2", TestStarted),
      NotifierEvent("An engine/1.2", TestFailed(classOf[ReasonInvalidException[_, _]])),
      NotifierEvent("An engine/1", TestFinished),
      NotifierEvent("An engine/2", TestStarted),
      NotifierEvent("An engine/2.1", TestStarted),
      NotifierEvent("An engine/2.1", TestFinished),
      NotifierEvent("An engine/2.2", TestStarted),
      NotifierEvent("An engine/2.2", TestFailed(classOf[CannotAddScenarioException[_, _]])),
      NotifierEvent("An engine/2.3", TestStarted),
      NotifierEvent("An engine/2.3", TestFailed(classOf[CannotAddScenarioException[_, _]])),
      NotifierEvent("An engine/2", TestFinished),
      NotifierEvent("An engine/index", TestFinished),
      NotifierEvent("org.cddcore.cddunit.ExampleJUnit", TestRunFinished)))
  }

  class RememberRunner extends RunNotifier {
    var descriptions = List[Description]()
    var events = List[NotifierEvent]()

    def holder = NotifierHolder(events.toVector.reverse)

    def popDescriptions = {
      val result = descriptions.head
      descriptions = descriptions.tail
      result
    }

    implicit def descriptionToId(description: Description) = Reflection(description).getFieldValue[java.io.Serializable]("fUniqueId").toString

    override def fireTestFailure(failure: Failure): Unit = {
      val description = popDescriptions
      events = NotifierEvent(description, TestFailed(failure.getException.getClass)) :: events
      super.fireTestFailure(failure)
    }

    override def fireTestFinished(description: Description): Unit = {
      val description = popDescriptions
      events = NotifierEvent(description, TestFinished) :: events
      super.fireTestFinished(description)
    }

    override def fireTestRunFinished(result: Result): Unit = {
      super.fireTestRunFinished(result)
      val description = popDescriptions
      events = NotifierEvent(description, TestRunFinished) :: events
    }

    override def fireTestRunStarted(description: Description): Unit = {
      descriptions = description :: descriptions
      events = NotifierEvent(description, TestRunStarted) :: events
      super.fireTestRunStarted(description)
    }

    override def fireTestStarted(description: Description): Unit = {
      super.fireTestStarted(description)
      descriptions = description :: descriptions
      events = NotifierEvent(description, TestStarted) :: events
    }
  }

  it should "allow checks against CddRunner notifications" in {
    val runner = new CddRunner(clazz)
    val notifier = new RememberRunner
    runner.run(notifier)
    notifier.holder shouldBe new NotifierHolder(notifierDsl.hierarchyBuilder.holder.events)

  }

}
