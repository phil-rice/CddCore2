package org.cddcore.cddunit

import org.cddcore.engine.Engine
import org.cddcore.engine.enginecomponents.{EngineComponent, Scenario, UseCase}
import org.cddcore.rendering.{RenderContext, Renderer}
import org.cddcore.utilities.{DisplayProcessor, Reflection, Strings}
import org.junit.runner.notification.{Failure, RunNotifier}
import org.junit.runner._

import scala.util.{Success, Try}

trait AbstractCddRunner extends Runner {

  protected def clazz: Class[Any]

  protected def engineData: Try[EngineData]

  protected def displayProcessor = DisplayProcessor.defaultDisplayProcessor

  class EngineData(val instance: HasEngines) {
    val engines = instance.engines

    implicit val dp = displayProcessor
    implicit val renderContext = Renderer.renderContext(engines: _*)
  }

  lazy val getDescription: Description = {
    val description = Description.createSuiteDescription(clazz)
    engineData match {
      case Success(engineData) =>
        import engineData._
        engines.foreach(e => description.getChildren.add(makeDescription(e)))
    }
    description
  }

  var ecToDescription = Map[EngineComponent[_, _], Description]()

  protected def makeDescription(ec: EngineComponent[_, _])(implicit renderContext: RenderContext): Description = {

    import renderContext._

    val id = renderContext.idPath(ec)
    def create(uc: UseCase[_, _]) = uc.components.foldLeft(Description.createSuiteDescription(Strings.cleanString(uc.title), id)) {
      (d, child) => d.getChildren.add(makeDescription(child));
        d
    }
    val result = ec match {
      case s: Scenario[_, _] => Description.createTestDescription(Strings.cleanString(s.toSummary(renderContext.displayProcessor)), id)
      case e: Engine[_, _] => create(e.asUseCase)
      case uc: UseCase[_, _] => create(uc)
    }
    ecToDescription = ecToDescription + (ec -> result)
    result
  }


  def run(notifier: RunNotifier): Unit = engineData match {
    case Success(ed) =>
      println("Starting the run")
      val result = new Result
      notifier.addListener(result.createListener)
      notifier.fireTestRunStarted(getDescription)
      for (engine <- ed.engines) {
        def run[P, R](engineD: Engine[_,_], ecd: EngineComponent[_,_]): Unit = {
          val engine =engineD.asInstanceOf[Engine[P,R]]
          val ec = ecd.asInstanceOf[EngineComponent[P,R]]
          val d = ecToDescription(ec)
          ec match {
            case s: Scenario[P, R] =>
              notifier.fireTestStarted(d)
              if (s.calcuateAssertionFor(engine, s.situation))
                notifier.fireTestFinished(d)
              else notifier.fireTestFailure(new Failure(d, new RuntimeException))
            case uc: UseCase[P, R] =>
              notifier.fireTestStarted(d)
              uc.components.foreach(e => run(engine, e))
              notifier.fireTestFinished(d)
            case e: Engine[P, R] =>
              notifier.fireTestStarted(d)
              e.asUseCase.components.foreach(e => run(engine, e))
              notifier.fireTestFinished(d)

          }
        }
        run(engine, engine)
      }
      notifier.fireTestRunFinished(result)


    case util.Failure(e) =>
      notifier.fireTestStarted(getDescription)
      notifier.fireTestFailure(new Failure(getDescription, e))
  }
}

class CddRunner(val clazz: Class[Any]) extends AbstractCddRunner {

  lazy val engineData = Try(new EngineData(Reflection.instantiate(clazz).asInstanceOf[HasEngines]))
}

trait HasEngines {
  def engines: List[Engine[_, _]]
}


trait CddContinuousIntegrationTest extends HasEngines


