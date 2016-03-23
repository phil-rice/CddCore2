package org.cddcore.utilities

import java.lang.reflect.Method

import scala.reflect.ClassTag

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
    Reflection(new ClassWIthPrivateField).getFieldValue[String]("privateField") shouldBe "somePrivateValue"
  }
  it should "allow the modifications of private fields in java objects, even thought this is probably a very bad thing to do" in {
    val instance = new ClassWIthPrivateField
    Reflection(instance).modField[String]("privateField") { x =>
      x shouldBe "somePrivateValue"
      "newValue"
    }
    Reflection(instance).getFieldValue[String]("privateField") shouldBe "newValue"
  }

  it should "allow that modification even of the field is in a parent class" in {
    class NewClass extends ClassWIthPrivateField
    val instance = new NewClass
    Reflection(instance).modField[String]("privateField") { x =>
      x shouldBe "somePrivateValue"
      "newValue"
    }
    Reflection(instance).getFieldValue[String]("privateField") shouldBe "newValue"
  }



  it should "return a list of all the fields" in {

    Reflection.getAllFields(classOf[ClassForTest]).map(_.getName) shouldBe
      List( "lvFour", "lvFive", "six", "bitmap$0","lvOne", "lvTwo", "three", "bitmap$0")
  }


  it should "return a map from fields to values that share a return type" in {

    Reflection(new ClassForTest).fieldMap[String].map { case (field, v) => (field.getName, v) } shouldBe
      Map("lvFour" -> "value4", "six" -> "value6", "lvOne" -> "value1", "three" -> "value3")

  }

  it should "return a map from fields to values that are suitably annotated" in {
    Reflection(new ClassForTest).fieldMapForAnnotation[TestAnnotation].map { case (field, v) => (field.getName, v) } shouldBe
      Map("lvFour" -> "value4", "three" -> "value3")

  }
}
