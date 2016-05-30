package org.cddcore.testinterface

import java.util.concurrent.atomic.AtomicInteger

import com.fasterxml.jackson.databind.ser.std.StdJdkSerializers.AtomicIntegerSerializer
import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.rendering.{JsonForRendering, Mustache, RenderConfiguration, Renderer}
import org.cddcore.utilities.{Indent, Iterables, Strings}
import org.junit.runner
import org.junit.runner.Description
import org.junit.runner.notification.{Failure, RunListener, RunNotifier}
import org.scalatools.testing.{EventHandler, _}

import scala.util._
import scala.util.Success

class CddFingerprint extends SubclassFingerprint {
  override def isModule: Boolean = false

  override def superClassName(): String = classOf[CddContinuousIntegrationTest].getName
}

class RunnerTracker {
  def finished = {
    val count = runnersFinished.incrementAndGet()
    if (count == runnersCreated.get)
      CddFramework.synchronized(count == runnersCreated.get)
    else
      false
  }

  def created = runnersCreated.incrementAndGet()

  private val runnersCreated = new AtomicInteger(0)
  private val runnersFinished = new AtomicInteger(0)

  override def toString: String = s"RunnerTracker(${runnersCreated.get},${runnersFinished.get}, $hashCode)"
}

object CddFramework {
}

class CddFramework extends org.scalatools.testing.Framework {

  val runnerTracker = new RunnerTracker

  override def name(): String = "CddUnit"

  override def tests(): Array[Fingerprint] = Array(new CddFingerprint)

  override def testRunner(testClassLoader: ClassLoader, loggers: Array[Logger]): Runner = {
    val logger: CddLogger = new CddLogger(loggers, Indent())
    CddFramework.synchronized {
      if (runnerTracker.created == 1) {
        logger.start(s"CddFramework")
      }
      new CddRunnerForTestInterface(testClassLoader, runnerTracker, logger)
    }
  }
}

class CddLogger(loggers: Array[Logger], val indent: Indent) {

  val logit = Iterables.guaranteedForeach(loggers) _

  def start(msg: String) = logit(_.info(indent + msg))

  def succeed(msg: String) = logit(_.info(indent + msg))

  def fail(msg: String) = logit(_.error(indent + msg))
}

class CddRunListener(eventHandler: EventHandler, runnerTracker: RunnerTracker, logger: CddLogger) extends RunListener {

  import logger._

  override def testStarted(description: Description): Unit = {
    indent.indent
    if (description.getChildren.size() > 0)
      start(s"$description")
  }

  override def testFinished(description: Description): Unit = {
    if (description.getChildren.size() == 0)
      succeed(s"- $description")
    indent.unindent
    eventHandler.handle(CddEvent(description.getDisplayName, description.toString, Result.Success))
  }

  override def testFailure(failure: Failure): Unit = {
    val description = failure.getDescription
    if (description.getChildren.size() == 0)
      fail(s"$description...failed")
    indent.unindent
    eventHandler.handle(CddEvent(description.getDisplayName, description.toString, Result.Failure))
  }

  override def testRunStarted(description: Description): Unit = {
    indent.indent
    start(s"$description")
  }

  override def testRunFinished(result: runner.Result): Unit = {
    super.testRunFinished(result)
    logger.indent.unindent
    if (runnerTracker.finished) {
      logger.succeed(s"and all finished ")
    }
  }

}

case class CddEvent(testName: String, description: String, result: Result, error: Throwable = null) extends Event

class CddRunnerForTestInterface(testClassLoader: ClassLoader, runnerTracker: RunnerTracker, cddLogger: CddLogger) extends org.scalatools.testing.Runner {
  override def run(testClassName: String, fingerprint: TestFingerprint, eventHandler: EventHandler, args: Array[String]): Unit = {
    val runner = new CddRunner(testClassLoader.loadClass(testClassName))
    val runNotifier = new RunNotifier

    runNotifier.addListener(new CddRunListener(eventHandler, runnerTracker, cddLogger))
    runner.run(runNotifier)
    val engines = runner.engineData.map(_.engines).getOrElse(List())
    implicit val renderConfiguration = RenderConfiguration.defaultRenderConfiguration
    renderConfiguration.urlManipulations.populateInitialFiles(renderConfiguration.referenceFilesUrlBase)

    val rc = engines.map(engine =>
      Renderer.makeReportFilesFor(testClassName, "../../reference", engine)
    ).last


    val classMap = Map[String, Any](
      "title" -> testClassName,
      "urlBase" -> ".",
      "engines" -> engines.map(e => Map("title" -> e.title)))

    val html = Mustache.apply("templates/TestClass.mustache")(classMap + ("json" -> JsonForRendering.pretty(classMap)))
    rc.urlManipulations.makeFile(Strings.uri(rc.urlBase,  "index.html"), html)
  }

}

