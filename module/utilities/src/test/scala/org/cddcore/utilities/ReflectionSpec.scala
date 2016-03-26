package org.cddcore.utilities

import java.lang.reflect.{Field, Method}

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class ToBeCreatedByReflection {
  val someValue = "someValue"

}

object ToBeCreatedByReflection {
  val someOtherValue = "someOverValue"
}


trait SuperTraitForTest {


  lazy val lvZero = 0

}

class SuperClassForTest {
  lazy val lvOne = "value1"
  lazy val lvTwo = 2
  @TestAnnotation
  val three = "value3"
}

class ClassForTest extends SuperClassForTest {
  @TestAnnotation
  lazy val lvFour = "value4"
  lazy val lvFive = 5
  val six = "value6"
}

class ClassForTestWithException extends ClassForTest {
  val e = new RuntimeException("some message")
  lazy val lvSeven: Int = throw e

}

class ReflectionSpec extends CddSpec {

  "The Reflection object" should "be able to create instances of a class" in {
    Reflection.instantiate(classOf[ToBeCreatedByReflection]).someValue shouldBe "someValue"
  }

  it should "be able to 'create' an object" in {
    val clazz = Class.forName("org.cddcore.utilities.ToBeCreatedByReflection$")
    val obj1 = Reflection.instantiate(clazz)
    val obj2 = Reflection.instantiate(clazz)
    obj1 shouldBe obj2
    val methods: Array[Method] = clazz.getDeclaredMethods
    withClue(methods)(methods.size shouldBe 1)
    methods(0).invoke(obj1) shouldBe "someOverValue"
    methods(0).invoke(obj2) shouldBe "someOverValue"
  }

  it should "allow the reading of private fields in java objects, even thought this is probably a very bad thing to do" in {
    Reflection(new ClassWIthPrivateField).getFieldValue[String]("privateField") shouldBe Success("somePrivateValue")
  }
  it should "allow the modifications of private fields in java objects, even thought this is probably a very bad thing to do" in {
    val instance = new ClassWIthPrivateField
    Reflection(instance).modField[String]("privateField") { x =>
      x shouldBe "somePrivateValue"
      "newValue"
    }
    Reflection(instance).getFieldValue[String]("privateField") shouldBe Success("newValue")
  }

  it should "allow that modification even of the field is in a parent class" in {
    class NewClass extends ClassWIthPrivateField
    val instance = new NewClass
    Reflection(instance).modField[String]("privateField") { x =>
      x shouldBe "somePrivateValue"
      "newValue"
    }
    Reflection(instance).getFieldValue[String]("privateField") shouldBe Success("newValue")
  }

  def getName[X](tuple: (Field, X)) = (tuple._1.getName, tuple._2)

  it should "return a list of all the fields" in {

    Reflection.getAllFields(classOf[ClassForTest]).map(_.getName) shouldBe
      List("lvFour", "lvFive", "six", "bitmap$0", "lvOne", "lvTwo", "three", "bitmap$0")
  }


  it should "return a map from fields to values that share a return type" in {

    Reflection(new ClassForTest).fieldMap[String].map(getName) shouldBe
      Map("lvFour" -> Success("value4"), "six" -> Success("value6"), "lvOne" -> Success("value1"), "three" -> Success("value3"))

  }

  it should "return a map from fields to values that are suitably annotated" in {
    Reflection(new ClassForTest).fieldMapForAnnotation[TestAnnotation].map {
      getName
    } shouldBe
      Map("lvFour" -> Success("value4"), "three" -> Success("value3"))
  }

  it should "return a map from fields to values including failures" in {
    val instance = new ClassForTestWithException
    val reflection: Reflection = Reflection(instance)
    reflection.fieldMap[Int].map(getName) shouldBe Map("lvSeven" -> Failure(instance.e), "lvFive" -> Success(5), "lvTwo" -> Success(2))
  }


  "A field map pimper" should "remove fields with a suitable annotation when getting a fieldMap" in {
    import Reflection._
    val instance = new ClassForTestWithException
    val reflection: Reflection = Reflection(instance)
    reflection.fieldMap[String].removeAnnotationFromFieldMap[TestAnnotation].map(getName) shouldBe Map("six" -> Success("value6"), "lvOne" -> Success("value1"))
    //    reflection.fieldMapIgnoring[Int, TestAnnotation].map(getName) shouldBe Map("lvSeven" -> Failure(instance.e), "lvFive" -> Success(5), "lvTwo" -> Success(2))
  }

  it should "sort based on allFields order" in {
    import Reflection._
    val r = Reflection(new ClassForTest)
    val sorted = r.fieldMap[String].sorted(r.allFields).map(getName)
    sorted.toList.map(_._1) shouldBe List("lvFour", "six", "lvOne", "three")
    sorted.keySet.toList shouldBe List("lvFour", "six", "lvOne", "three")
  }

  it should "return a map for fields to a success of string saying what has happened" in {
    import Reflection._
    val reflection: Reflection = Reflection(new ClassForTest)
    val raw: Map[Field, Try[Int]] = reflection.fieldMap[Int]
    raw.displayStringMap((i: Int) => s"i_$i").map(getName) shouldBe Map("lvFive" -> Success("i_5"), "lvTwo" -> Success("i_2"))
  }
  it should "return a map for fields to a string indicating the failure, if the value could not be got" in {
    import Reflection._
    val reflection: Reflection = Reflection(new ClassForTestWithException)
    val raw: Map[Field, Try[Int]] = reflection.fieldMap[Int]
    raw.displayStringMap((i: Int) => s"i_$i").map(getName) shouldBe Map(
      "lvSeven" -> Success("<Error>RuntimeException/some message</error>"),
        "lvFive" -> Success("i_5"),
        "lvTwo" -> Success("i_2"))
  }
}
