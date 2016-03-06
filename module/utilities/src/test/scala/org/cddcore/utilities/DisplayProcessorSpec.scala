package org.cddcore.utilities

class DisplayProcessorSpec extends CddSpec {

   class SomeThing(value1: String, value2: Int){
     override def toString = s"SomeThing($value1,$value2)"
   }
  case class SomeThingWithTraits(value1: String, value2: Int) extends SomeThing(value1, value2) with ToHtml with ToDetailed with ToSummary{
     def toHtml(displayProcessor: DisplayProcessor) = s"toHtml($this)"

     def toDetailed(displayProcessor: DisplayProcessor)= s"toDetailed($this)"

     def toSummary(displayProcessor: DisplayProcessor)= s"toSummary($this)"
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
    dp.summarize(st1) shouldBe "SomeThing(one,1)"
    dp.html(st1) shouldBe "SomeThing(one,1)"
    dp.detailed(st1) shouldBe "SomeThing(one,1)"
  }
  "The default display processor" should "use a function is specified" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summarize(st1) shouldBe "Sum(SomeThing(one,1))mary"
    dp.html(st1) shouldBe "Ht(SomeThing(one,1))ml"
    dp.detailed(st1) shouldBe "Det(SomeThing(one,1))ailed"

    remembered shouldBe Set(dp)
  }

  it should "use the toString if the partial functions don't match" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summarize(1) shouldBe "1"
    dp.html(2) shouldBe "2"
    dp.detailed(3) shouldBe "3"
    remembered shouldBe Set()
  }

  it should "use traits if they exist preferentially" in {
    val f = new DisplayProcessorFramework
    import f._
    dp.summarize(st1t) shouldBe "toSummary(SomeThing(one,1))"
    dp.html(st1t) shouldBe "toHtml(SomeThing(one,1))"
    dp.detailed(st1t) shouldBe "toDetailed(SomeThing(one,1))"

    remembered shouldBe Set()
  }



}
