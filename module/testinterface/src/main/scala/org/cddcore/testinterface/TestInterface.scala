/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.testinterface

import java.util.concurrent.atomic.AtomicInteger

import com.fasterxml.jackson.databind.ser.std.StdJdkSerializers.AtomicIntegerSerializer
import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.engine.Engine
import org.cddcore.rendering._
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
  def finishAndExecuteIfLast(block: => Unit) = CddFramework.synchronized {
    val count = runnersFinished.incrementAndGet()
    if (count == runnersCreated.get) block
  }

  def created = runnersCreated.incrementAndGet()

  private val runnersCreated = new AtomicInteger(0)
  private val runnersFinished = new AtomicInteger(0)


  override def toString: String = s"RunnerTracker(${runnersCreated.get},${runnersFinished.get}, $hashCode)"
}


object CddFramework {
  val runnerTracker = new RunnerTracker
  val testStructure = new TestStructure(new TestViews(new TestIndexView, new TestClassView))


}

class CddFramework extends org.scalatools.testing.Framework {

  import CddFramework._

  override def name(): String = "CddUnit"

  override def tests(): Array[Fingerprint] = Array(new CddFingerprint)

  override def testRunner(testClassLoader: ClassLoader, loggers: Array[Logger]): Runner = {
    val logger: CddLogger = new CddLogger(loggers, Indent())
    CddFramework.synchronized {
      if (runnerTracker.created == 1) {
        logger.start(s"CddFramework")
      }
      new CddRunnerForTestInterface(testClassLoader, runnerTracker, testStructure, logger)
    }
  }
}

class CddLogger(loggers: Array[Logger], val indent: Indent) {

  val logit = Iterables.guaranteedForeach(loggers) _

  def start(msg: String) = logit(_.info(indent + msg))

  def succeed(msg: String) = logit(_.info(indent + msg))

  def fail(msg: String) = logit(_.error(indent + msg))
}

class CddRunListener(eventHandler: EventHandler, runnerTracker: RunnerTracker, logger: CddLogger)(whenFinished: => Unit) extends RunListener {

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

    runnerTracker.finishAndExecuteIfLast {
      whenFinished
    }
  }
}

case class CddEvent(testName: String, description: String, result: Result, error: Throwable = null) extends Event

class CddRunnerForTestInterface(testClassLoader: ClassLoader, runnerTracker: RunnerTracker, testStructure: TestStructure, cddLogger: CddLogger) extends org.scalatools.testing.Runner {
  override def run(testClassName: String, fingerprint: TestFingerprint, eventHandler: EventHandler, args: Array[String]): Unit = {
    val runner = new CddRunner(testClassLoader.loadClass(testClassName))
    val engines = runner.engineData.map(_.engines).getOrElse(List())
    testStructure.add(testClassName, engines)


    val runNotifier = new RunNotifier
    runNotifier.addListener(new CddRunListener(eventHandler, runnerTracker, cddLogger)(testStructure.makeFiles))
    runner.run(runNotifier)

//    testStructure.makeFiles
  }
}

