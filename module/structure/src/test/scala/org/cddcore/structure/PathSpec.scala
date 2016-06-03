package org.cddcore.structure

import org.cddcore.utilities.CddSpec

import scala.util.{Failure, Success}
import scala.xml.Node


class PathResultSpec extends CddSpec {
  "A pathresult resultToAggregate method" should "delegate the method to its strategy" in {
    val strategy = smartMock[PathResultStrategy[String, Int]]
    val pathResult = new PathResult[String, Int, List[Any]](mock[(Int) => List[Any]], strategy)
    strategy.resultToAggregate(List("1")) returns Success(1)
    pathResult.resultToAggregate(List("1")) shouldBe Success(1)
  }
  it should "delegate calls to apply to its convertor" in {
    val convertor = mock[(Int) => List[Any]]
    convertor.apply(1) returns List(1)
    val pathResult = new PathResult[String, Int, List[Any]](convertor, smartMock[PathResultStrategy[String, Int]])

    pathResult.apply(1) shouldBe List(1)
  }
}

class PathSpec extends CddSpec {

  def withPath(fn: Path[String, Int, List[Any]] => Structure[String] => PathRootAndSteps[String] => PathResult[String, Int, List[Any]] => Unit) = {
    implicit val structure = smartMock[Structure[String]]
    val pathRootAndSteps = smartMock[PathRootAndSteps[String]]
    val pathResult = mock[PathResult[String, Int, List[Any]]]
    val path = new Path[String, Int, List[Any]](pathRootAndSteps, pathResult, false)
    fn(path)(structure)(pathRootAndSteps)(pathResult)
  }

  val found = List("someFoundItems")
  val aggregate = 1
  val result = List[Any]("result")
  val runtimeException = new RuntimeException("someMessage")

  "A path get method" should "find the result by passing pathRootAndSteps to the structure, then aggregate it, and map it through the convertor" in {
    withPath { path => structure => pathRootAndSteps => pathResult =>
      structure.findResult(pathRootAndSteps) returns found
      pathResult.resultToAggregate(found) returns Success(aggregate)
      pathResult.apply(aggregate) returns result

      path.get shouldBe Success(result)
    }
  }

  "A path apply method" should "return the same as the get followed by a get" in {
    withPath { path => structure => pathRootAndSteps => pathResult =>
      structure.findResult(pathRootAndSteps) returns found
      pathResult.resultToAggregate(found) returns Success(aggregate)
      pathResult.apply(aggregate) returns result

      path.apply() shouldBe result
    }
  }

  it should "give a nice error message if the result was an error" in {
    withPath { path => structure => pathRootAndSteps => pathResult =>
      structure.findResult(pathRootAndSteps) returns found
      pathResult.resultToAggregate(found) returns Failure(runtimeException)
      pathResult.apply(aggregate) returns result

      intercept[RuntimeException](path.apply()) shouldBe runtimeException
    }
  }
}

class PathResultsSpec extends CddSpec {

  def withPathResultsAndMockStructure(fn: PathResults[String] => Structure[String] => Unit) = {
    val mockStructure = smartMock[Structure[String]]
    val pathResults = new PathResults[String] {
      implicit def structure = mockStructure
    }
    fn(pathResults)(mockStructure)
  }

  val xml = new Xml
  val noNodes: Seq[Node] = List[Node]()
  val oneNode: Seq[Node] = <Hello>SomeText</Hello>
  val listOfNodes: Seq[Node] = List(<Hello>SomeText</Hello> , <Hello>SomeMoreText</Hello>)

  "Aggregate Strings" should "foldLeft on the results, aggregating results into a string builder" in {
    xml.AggregateStrings.resultToAggregate(noNodes) shouldBe Success("")
    xml.AggregateStrings.resultToAggregate(oneNode) shouldBe Success("SomeText")
    xml.AggregateStrings.resultToAggregate(listOfNodes) shouldBe Success("SomeTextSomeMoreText")
  }
  "Aggregate Option Strings" should "return None or the Some of the aggregate of the strings " in {
    xml.AggregateOptionString.resultToAggregate(noNodes) shouldBe Success(None)
    xml.AggregateOptionString.resultToAggregate(oneNode) shouldBe Success(Some("SomeText"))
    xml.AggregateOptionString.resultToAggregate(listOfNodes) shouldBe Success(Some("SomeTextSomeMoreText"))
  }
  "OneAndOnlyOneString " should "return a string or throw a StringNotFoundInStructureException " in {
    xml.OneAndOnlyOneString.resultToAggregate(oneNode) shouldBe Success("SomeText")
    
    intercept[StringNotFoundInStructureException](xml.OneAndOnlyOneString.resultToAggregate(noNodes)) shouldBe StringNotFoundInStructureException(noNodes)
    intercept[StringNotFoundInStructureException](xml.OneAndOnlyOneString.resultToAggregate(listOfNodes)) shouldBe StringNotFoundInStructureException(listOfNodes)
  }

}
