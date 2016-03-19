package org.cddcore.utilities

import java.lang.reflect.Method

import scala.reflect.ClassTag

class ToBeCreatedByReflection {
  val someValue = "someValue"

}

object ToBeCreatedByReflection {
  val someOtherValue = "someOverValue"
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
    val instance = new ClassWIthPrivateField
    Reflection.getFieldValue[String](instance, "privateField") shouldBe "somePrivateValue"
  }
  it should "allow the modifications of private fields in java objects, even thought this is probably a very bad thing to do" in {
    val instance = new ClassWIthPrivateField
    Reflection.modField[String](instance, "privateField") { x =>
      x shouldBe "somePrivateValue"
      "newValue"
    }
    Reflection.getFieldValue[String](instance, "privateField") shouldBe "newValue"
  }
}
