package org.cddcore.cddunit

import org.cddcore.utilities.{Hierarchy, MutableHierarchyBuilderWithChildLifeCycle, CddSpec}

trait JunitNotification

object TestRunStarted extends JunitNotification

object TestRunFinished extends JunitNotification

object TestStarted extends JunitNotification

object TestFinished extends JunitNotification

case class TestFailed(clazz: Class[_ <: Exception])

trait NotifierData

case class NotifierEvent(name: String, descriptionData: DescriptionData, notification: JunitNotification) extends NotifierData

case class NotifierHolder(events: Vector[NotifierData]) extends NotifierData

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
    addChild(NotifierEvent("", descriptionDsl.description, TestRunStarted))
    block
    addChild(NotifierEvent("", descriptionDsl.description, TestRunFinished))
  }

  def testSuite(useCaseName: String)(block: => Unit): Unit = {
    val id = renderContext.pathMap(descriptionDsl.findUsecaseWithName(useCaseName))
    descriptionDsl.description.allChildrenIncludingMe.find(_.uniqueId == id) match {
      case Some(d) =>
        addChild(NotifierEvent(s"testSuite(${useCaseName})", d, TestStarted))
        block
        addChild(NotifierEvent(s"testSuite(${useCaseName})", d, TestFinished))
    }
  }

  def scenario(p: P) = {
    val id = renderContext.pathMap(descriptionDsl.findScenarioWithSituation(p))
    descriptionDsl.description.allChildrenIncludingMe.find(_.uniqueId == id) match {
      case Some(d) =>
        addChild(NotifierEvent(s"scenario(${p})", d, TestStarted))
        addChild(NotifierEvent(s"scenario(${p})", d, TestFinished))
    }
  }
}

class NotifierDslSpec extends CddSpec with DescriptionDslTestFramework {

  "The NotifierDsl" should "make easy to read lists of notifications" in {
    val notifierDsl = new NotifierDsl[Int, String](dsl) {
      testRun {
        testSuite("a use case") {
          scenario(1)
          scenario(2)
        }
        testSuite("another use case") {
          scenario(3)
          scenario(4)
          scenario(5)
        }
      }
    }
  }

}
