/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{Document, InternetDocument, Reference, UseCase}

import scala.collection.immutable.ListMap


class EngineSpec extends CddEngineSpec {

  case class Person(wealth: Int)

  type UC = UseCase[Person, String]
  "The Engine's title" should "be defined by the constructor" in {
    new Engine("someTitle").title shouldBe "someTitle"
    new Engine().title shouldBe "Untitled"
  }

  it should "be settable" in {
    new Engine("someTitle") {
      title("someOtherTitle")
    }.title shouldBe "someOtherTitle"
  }

  "An engine " should "have use cases added " in {
    val e = new Engine[Person, String] {
      useCase("some usecase1")()
      useCase("some usecase2", "comment")()
      useCase("usecase3")()
    }
    val List(uc3, uc2, uc1) = e.asUseCase.components
    uc1 shouldBe UseCase[Person, String]("some usecase1", List(), None, uc1.definedInSourceCodeAt, ListMap(), List())
    uc2 shouldBe UseCase[Person, String]("some usecase2", List(), Some("comment"), uc2.definedInSourceCodeAt, ListMap(), List())
    uc3 shouldBe UseCase[Person, String]("usecase3", List(), None, uc3.definedInSourceCodeAt, ListMap(), List())

    uc1.definedInSourceCodeAt.toString shouldBe "(EngineSpec.scala:26)"
    uc2.definedInSourceCodeAt.toString shouldBe "(EngineSpec.scala:27)"
    uc3.definedInSourceCodeAt.toString shouldBe "(EngineSpec.scala:28)"
  }

  it should "be possible to nest use cases" in {
    val e = safeMake(new Engine[Person, String] {
      useCase("usecase1") {
        useCase("usecase1a", "comment")()
      }
      useCase("usecase2") {
        useCase("usecase2a")()
        useCase("usecase2b")()
      }
    })
    val List(uc2: UC, uc1: UC) = e.asUseCase.components
    val (List(uc1a: UC)) = uc1.components
    val (List(uc2b: UC, uc2a: UC)) = uc2.components
    uc1.title shouldBe "usecase1"
    uc2.title shouldBe "usecase2"
    uc1a.title shouldBe "usecase1a"
    uc2a.title shouldBe "usecase2a"
    uc2b.title shouldBe "usecase2b"
  }
  val e = new Engine[Person, String] {
    useCase("rich people") {
      Person(1000) produces "accept" when (_.wealth >= 1000)
      Person(2000) produces "accept"
    }
    useCase("poor people") {
      Person(100) produces "reject"
      Person(200) produces "reject"
    }
  }

  "Adding scenarios to a usecase" should "appear in the use case" in {
    val List(poorUsecase: UC, richUseCase: UC) = e.asUseCase.components
    richUseCase.title shouldBe "rich people"
    val List(rich1000, rich2000) = richUseCase.allScenarios
    val List(poor100, poor200) = poorUsecase.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)

  }
  "Adding scenarios to a usecase" should "appear in allScenarios" in {
    val List(rich1000, rich2000, poor100, poor200) = e.allScenarios
    rich1000.situation shouldBe Person(1000)
    rich2000.situation shouldBe Person(2000)
    poor100.situation shouldBe Person(100)
    poor200.situation shouldBe Person(200)
  }

  "Adding references to a usecase" should "be remembered by the use case" in {
    val d1 = Document.internet("some ref 1")
    val d2 = Document.internet("some ref 2")
    val e = new Engine[Int, String] {
      useCase("useCase1", "Comment1", references = List(Reference(d1))) {
        1 produces "one"
      }
      useCase("useCase2", "Comment2", references = List(Reference(d2))) {
        2 produces "two"
      }
    }
    val List(uc2: UseCase[Int, String], uc1: UseCase[Int, String]) = e.asUseCase.components
    uc1.references shouldBe List(Reference(d1))
    uc2.references shouldBe List(Reference(d2))
  }

  "References for an engine" should "be remembered by the engine" in {
    val d1 = Document.internet("some ref 1")
    val d2 = Document.internet("some ref 2")
    val e = new Engine[Int, String](references = List(Reference(d1), Reference(d2))) {
      useCase("useCase1", "Comment1") {
        1 produces "one"
      }
      useCase("useCase2", "Comment2") {
        2 produces "two"
      }
    }
    e.references shouldBe List(Reference(d1), Reference(d2))
  }

  they should "be aggregated in the documents field of engine" in {
    val d1 = Document.internet("some ref 1")
    val d2 = Document.internet("some ref 2")
    val d3 = Document.internet("some ref 3")
    val d4 = Document.internet("some ref 4")
    val e = new Engine[Int, String](references = List(Reference(d1), Reference(d2, "2"))) {
      useCase("useCase1", "Comment1", references = List(Reference(d3, "3"))) {
        1 produces "one" ref d4
      }
      useCase("useCase2", "Comment2") {
        2 produces "two" ref(d4, "4")
      }
    }
    e.references shouldBe List(Reference(d1), Reference(d2, "2"))
    e.allReferences.toSet shouldBe Set(Reference(d1), Reference(d2, "2"), Reference(d3, "3"), Reference(d4), Reference(d4, "4"))
    e.allDocuments.toSet shouldBe Set(d1, d2, d3, d4)
  }
}
