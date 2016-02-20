package org.cddcore.utilities

import scala.language.experimental.macros
import scala.language.implicitConversions

class CodeHolderSpec extends CddSpec {

  import CodeHolder._

  val horribleToString =
    """{
      |SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[String,String] with Serializable {
      | def <init>(): <$anon: String => String> = {
      |   $anonfun.super.<init>();
      |   ()
      | };
      | final override def applyOrElse[A1 <: String, B1 >: String](x2: A1, default: A1 => B1): B1 = ((x2.asInstanceOf[String]: String): String @unchecked) match {
      |   case (x @ (_: String)) => x.toString()
      |   case (defaultCase$ @ _) => default.apply(x2)
      | };
      | final def isDefinedAt(x2: String): Boolean = ((x2.asInstanceOf[String]: String): String @unchecked) match {
      |   case (x @ (_: String)) => true
      |   case (defaultCase$ @ _) => false
      | }
      |;
      |ew $anonfun()
      |PartialFunction[String,String]])""".stripMargin

  "A code holder" should "have a decent toString for simple functions" in {
    val c: CodeHolder[String => String] = (x: String) => x.toString
    c.description shouldBe "((x: String) => x.toString())"
  }

  it should "turn horrible toStrings of partial functions into nice ones" in {
    CodeHolder.partialFnToStringToNiceToString(horribleToString) shouldBe Some("{case (x @ (_: String)) => x.toString()}")
  }

  it should "have a decent prettyDescription for partial functions" in {
    CodeHolder.fnToHolder[String => String] { case x => x.toString }.prettyDescription shouldBe "case (x @ _) => x.toString()"
    CodeHolder.fnToHolder[Int => String] { case x: Int => x.toString }.prettyDescription shouldBe "case (x @ (_: Int)) => x.toString()"
  }

}