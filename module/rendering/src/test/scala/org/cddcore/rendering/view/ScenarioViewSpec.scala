package org.cddcore.rendering.view

import org.cddcore.engine.{Engine, InvalidScenariosTestFramework}
import org.cddcore.enginecomponents.{Document, Reference, Scenario}
import org.cddcore.rendering.Renderer


class ScenarioViewSpec extends InvalidScenariosTestFramework {

  import View._

  implicit val rc = Renderer.renderContext(allEngines: _*)

  def withView(fn: (ScenarioView, LinkView, ExceptionView) => Unit) = {
    val linkView: LinkView = new LinkView(new IconUrlFinder)
    val exceptionView = new ExceptionView
    val view = new ScenarioView(linkView, exceptionView)
    fn(view, linkView, exceptionView)
  }

  "ScenarioView" should "create a map with the scenario details in it, with no " in {
    withView { (scenarioView, linkView, exceptionView) =>
      val s: Scenario[String, String] = scenario("conflicting1")
      val map = scenarioView(s)
      map(Id) shouldBe rc.idPath(s)
      map(Type) shouldBe ScenarioTypeName
      map(linkKey) shouldBe linkView(s)
      map(Title) shouldBe s.title
      map(summaryKey) shouldBe s.summary(rc.displayProcessor)
      map(Comment) shouldBe ""
      map(situationKey) shouldBe s.situation
      map(expectedKey) shouldBe "result"
      map(References) shouldBe List()
      map.get(Error) shouldBe None
    }
  }

  it should "add references if they exist" in {
    withView { (scenarioView, linkView, exceptionView) =>
      val doc1 = Document.paper("someDocName1")
      val doc2 = Document.paper("someDocName2")
      val e = new Engine[Int, String] {
        1 produces "result" ref(doc1, "one") ref (doc2)
      }
      val rc = Renderer.renderContext(e)
      val Seq(s) = e.allScenarios
      val map = scenarioView(s)(rc)
      map(References) shouldBe List(
        Map("document" -> Map("name" -> "someDocName1", "ref" -> ""), "internalRef" -> Some("one"), "imgSrc" -> "./target/cddimages/document.png"),
        Map("document" -> Map("name" -> "someDocName2", "ref" -> ""), "internalRef" -> None, "imgSrc" -> "./target/cddimages/document.png"))
    }
  }

  it should "add comments if they exist" in {
    withView { (scenarioView, linkView, exceptionView) =>
      val doc1 = Document.paper("someDocName1")
      val doc2 = Document.paper("someDocName2")
      val e = new Engine[Int, String] {
        1 produces "result" withComment ("someComment")
      }
      val rc = Renderer.renderContext(e)
      val Seq(s) = e.allScenarios
      val map = scenarioView(s)(rc)
      map(Comment) shouldBe "someComment"
    }
  }

  it should "add errors if they exist" in {
    withView { (scenarioView, linkView, exceptionView) =>
      val s: Scenario[String, String] = scenario("conflicting2")
      val map = scenarioView(s)
      map(Error) shouldBe exceptionView(exception("conflicting2"))
    }
  }
}
