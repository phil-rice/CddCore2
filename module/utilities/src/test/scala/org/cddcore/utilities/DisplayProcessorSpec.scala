/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.utilities

class DisplayProcessorSpec extends CddSpec {

  class SomeThing(value1: String, value2: Int) {
    override def toString = s"SomeThing($value1,$value2)"
  }

  case class SomeThingWithTraits(value1: String, value2: Int) extends SomeThing(value1, value2) with ToHtml with ToDetailed with ToSummary {
    def html(implicit displayProcessor: DisplayProcessor) = s"toHtml($this)"

    def detailed(implicit displayProcessor: DisplayProcessor) = s"toDetailed($this)"

    def summary(implicit displayProcessor: DisplayProcessor) = s"toSummary($this)"
  }

  val st1 = new SomeThing("one", 1)
  val st2 = new SomeThing("two", 2)
  val st1t = new SomeThingWithTraits("one", 1)
  val st2t = new SomeThingWithTraits("two", 2)

  class DisplayProcessorFramework {
    var remembered = Set[DisplayProcessor]()

    def fn(prefix: String, postfix: String): PartialFunction[(DisplayProcessor, Any), String] = {
      case (d: DisplayProcessor, x: SomeThing) =>
        remembered += d
        prefix + x + postfix
    }

    val dp = DisplayProcessor.defaultDisplayProcessor.withDetailer(fn("Det(", ")ailed")).withHtml(fn("Ht(", ")ml")).withSummarize(fn("Sum(", ")mary"))

  }

  "The default display processor" should "return the toString of the passed in thing" in {
    val dp = DisplayProcessor.defaultDisplayProcessor
    dp.summary(st1) shouldBe "SomeThing(one,1)"
    dp.html(st1) shouldBe "SomeThing(one,1)"
    dp.detailed(st1) shouldBe "SomeThing(one,1)"
  }
  "The default display processor" should "use a function is specified" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summary(st1) shouldBe "Sum(SomeThing(one,1))mary"
    dp.html(st1) shouldBe "Ht(SomeThing(one,1))ml"
    dp.detailed(st1) shouldBe "Det(SomeThing(one,1))ailed"

    remembered shouldBe Set(dp)
  }

  it should "use the toString if the partial functions don't match" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summary(1) shouldBe "1"
    dp.html(2) shouldBe "2"
    dp.detailed(3) shouldBe "3"
    remembered shouldBe Set()
  }

  it should "use traits if they exist preferentially" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summary(st1t) shouldBe "toSummary(SomeThing(one,1))"
    dp.html(st1t) shouldBe "toHtml(SomeThing(one,1))"
    dp.detailed(st1t) shouldBe "toDetailed(SomeThing(one,1))"

    remembered shouldBe Set()
  }

  it should "open up tuple2" in {
    val f = new DisplayProcessorFramework
    import f._
    val t1 = (st1, st2)
    dp.summary(t1) shouldBe "(Sum(SomeThing(one,1))mary,Sum(SomeThing(two,2))mary)"
    dp.html(t1) shouldBe "(Ht(SomeThing(one,1))ml,Ht(SomeThing(two,2))ml)"
    dp.detailed(t1) shouldBe "(Det(SomeThing(one,1))ailed,Det(SomeThing(two,2))ailed)"

    remembered shouldBe Set(dp)
  }

  it should "open up tuple3" in {
    val f = new DisplayProcessorFramework
    import f._
    val t1 = (st1, st2, st1t)
    dp.summary(t1) shouldBe "(Sum(SomeThing(one,1))mary,Sum(SomeThing(two,2))mary,toSummary(SomeThing(one,1)))"
    dp.html(t1) shouldBe "(Ht(SomeThing(one,1))ml,Ht(SomeThing(two,2))ml,toHtml(SomeThing(one,1)))"
    dp.detailed(t1) shouldBe "(Det(SomeThing(one,1))ailed,Det(SomeThing(two,2))ailed,toDetailed(SomeThing(one,1)))"

    remembered shouldBe Set(dp)

  }

  it should "open up list" in {
    val f = new DisplayProcessorFramework
    import f._
    val list = List(st1, st2, st1t)
    dp.summary(list) shouldBe "List(Sum(SomeThing(one,1))mary,Sum(SomeThing(two,2))mary,toSummary(SomeThing(one,1)))"
    dp.html(list) shouldBe "List(Ht(SomeThing(one,1))ml,Ht(SomeThing(two,2))ml,toHtml(SomeThing(one,1)))"
    dp.detailed(list) shouldBe "List(Det(SomeThing(one,1))ailed,Det(SomeThing(two,2))ailed,toDetailed(SomeThing(one,1)))"

    remembered shouldBe Set(dp)
  }
  it should "open up map" in {
    val f = new DisplayProcessorFramework
    import f._
    val list = Map(st1 -> st2)
    dp.summary(list) shouldBe "Map(Sum(SomeThing(one,1))mary -> Sum(SomeThing(two,2))mary)"
    dp.html(list) shouldBe "Map(Ht(SomeThing(one,1))ml -> Ht(SomeThing(two,2))ml)"
    dp.detailed(list) shouldBe "Map(Det(SomeThing(one,1))ailed -> Det(SomeThing(two,2))ailed)"

    remembered shouldBe Set(dp)
  }

  it should "prefer an explicit summarizer to the default map behaviour" in {
    val dp = DisplayProcessor.defaultDisplayProcessor.withSummarize { case (dp, x: Map[_, _]) => "ProcessingMap" }
    val list = Map(st1 -> st2)
    dp.summary(list) shouldBe "ProcessingMap"

  }


  it should "display 'displayables appropriately" in {
    case class Demo(s: String) extends Displayable {
      def detailed(implicit dp: DisplayProcessor): String = s"(($s))"

      override def summary(implicit dp: DisplayProcessor): String = s"[[$s]]"

      override def html(implicit dp: DisplayProcessor): String = s"{{$s}}"
    }
    val dp = DisplayProcessor.defaultDisplayProcessor
    dp.html(Demo("hello")) shouldBe "{{hello}}"
    dp.summary(Demo("hello")) shouldBe "[[hello]]"
    dp.detailed(Demo("hello")) shouldBe "((hello))"
  }

}
