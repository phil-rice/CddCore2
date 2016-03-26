package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.cddcore.enginecomponents.{DefinedInSourceCodeAt, EngineComponent, Scenario, UseCase}
import org.cddcore.rendering.{RenderContext, Renderer}
import org.cddcore.utilities.{DisplayProcessor, Reflection, Strings}
import org.junit.runner._
import org.junit.runner.notification.{Failure, RunNotifier}

import scala.util.{Success, Try}

trait AbstractCddRunner extends Runner {

  protected def clazz: Class[_]

  protected def engineData: Try[EngineData]

  protected def displayProcessor = DisplayProcessor.defaultDisplayProcessor

  class EngineData(val instance: HasEngines) {
    val engines = instance.engines

    implicit val dp = displayProcessor
    implicit val renderContext = Renderer.renderContext(engines: _*)
  }

  var buildExceptionOption: Option[Throwable] = None
  lazy val getDescription: Description = {
    val description = Description.createSuiteDescription(clazz)
    engineData match {
      case Success(engineData) =>
        import engineData._
        engines.foreach(e => description.getChildren.add(makeDescription(e)))
      case util.Failure(e) => buildExceptionOption = Some(e)
    }
    description
  }

  var ecToDescription = Map[EngineComponent[_, _], Description]()

  protected def makeDescription(ec: EngineComponent[_, _])(implicit renderContext: RenderContext): Description = {

    val id = renderContext.idPath(ec)
    def create(uc: UseCase[_, _]) = uc.components.reverse.foldLeft(Description.createSuiteDescription(Strings.cleanStringForJunitName(uc.title), id)) {
      (d, child) => d.getChildren.add(makeDescription(child));
        d
    }
    val result = ec match {
      case s: Scenario[_, _] => Description.createSuiteDescription(Strings.cleanStringForJunitName(s.toSummary(renderContext.displayProcessor)), id)
      case e: Engine[_, _] => create(e.asUseCase)
      case uc: UseCase[_, _] => create(uc)
    }
    ecToDescription = ecToDescription + (ec -> result)
    result
  }


  def run(notifier: RunNotifier): Unit = {
    def runTest(d: Description)(block: => Boolean): Boolean = {
      //      println(s"runTest: $d")
      notifier.fireTestStarted(d)
      try {
        val result = block
        if (result) notifier.fireTestFinished(d)
        else
          notifier.fireTestFailure(new Failure(d, new RuntimeException))
        result
      } catch {
        case e: Exception => notifier.fireTestFailure(new Failure(d, e)); false
      }
    }
    buildExceptionOption match {
      case Some(e) =>
        val result = new Result
        notifier.addListener(result.createListener)
        notifier.fireTestRunStarted(getDescription)
        notifier.fireTestStarted(getDescription)
        notifier.fireTestFailure(new Failure(getDescription, e))
        notifier.fireTestRunFinished(result)
      case None =>
        engineData match {
          case Success(ed) =>
            //        println("Starting the run")
            val result = new Result
            notifier.addListener(result.createListener)
            notifier.fireTestRunStarted(getDescription)
            ed.engines.foreach { engine =>
              engine.decisionTree
              //          println(s"... error scenarios\n.....${engine.errors.mkString("\n.....")}")
              def run[P, R](engineD: Engine[_, _], ecd: EngineComponent[_, _]): Boolean = {
                val engine = engineD.asInstanceOf[Engine[P, R]]
                val ec = ecd.asInstanceOf[EngineComponent[P, R]]
                val d = ecToDescription(ec)
                ec match {
                  case s: Scenario[P, R] if engine.errors.contains(s) => notifier.fireTestStarted(d); notifier.fireTestFailure(new Failure(d, CddRunner.modifyException(engine.errors(s), s.definedInSourceCodeAt))); true
                  case s: Scenario[P, R] => runTest(d)(s.calcuateAssertionFor(engine, s.situation))
                  case uc: UseCase[P, R] => runTest(d)(uc.components.reverse.foldLeft(true)((acc, c) => run(engine, c) && acc))
                  case e: Engine[P, R] => {
                    val result = runTest(d)(e.asUseCase.components.reverse.foldLeft(true)((acc, c) => run(engine, c) && acc))
                    CddRunner.makeReports(clazz.getName, engine)
                    result
                  }
                }
              }
              //          println(s"Running engine ${engine.title}")
              run(engine, engine)
            }
            notifier.fireTestRunFinished(result)

          case util.Failure(e) =>
            notifier.fireTestStarted(getDescription)
            notifier.fireTestFailure(new Failure(getDescription, e))
        }
    }
  }
}

object CddRunner {

  def makeReports[P, R](urlOffset: String, engine: Engine[P, R]) = try {
    //    println(s"MakingAReport for $urlOffset and engine ${engine.title}")
    Renderer.makeReportFilesFor(urlOffset, engine)
  } catch {
    case e: Exception => println(e); e.printStackTrace()
  }


  def modifyException[E <: Exception](e: E, definedInSourceCodeAt: DefinedInSourceCodeAt): E = {
    e.getStackTrace
    Reflection(e).modField[Array[StackTraceElement]]("stackTrace") { oldSt =>
      if (oldSt.size == 0 || oldSt(0) == definedInSourceCodeAt.st)
        oldSt
      else
        Array(definedInSourceCodeAt.st) ++ oldSt
    }
    e
  }

}

class CddRunner(val clazz: Class[_]) extends AbstractCddRunner {

  lazy val engineData = Try(new EngineData(Reflection.instantiate(clazz).asInstanceOf[HasEngines]))
}

trait HasEngines {
  def engines: List[Engine[_, _]]
}


trait CddContinuousIntegrationTest extends HasEngines


