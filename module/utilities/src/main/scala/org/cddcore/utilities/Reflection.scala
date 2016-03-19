package org.cddcore.utilities

import java.lang.reflect.Field

import scala.reflect.ClassTag

object Reflection {

  def getFieldValue[T: ClassTag](instance: Any, fieldName: String) = {
    val field: Field = getField(instance, fieldName)
    field.get(instance).asInstanceOf[T]
  }

  def modField[T: ClassTag](instance: Any, fieldName: String)(fn: T => T) = {
    val field = getField[T](instance, fieldName)
    val oldValue = field.get(instance).asInstanceOf[T]
    val newValue = fn(oldValue)
    field.set(instance, newValue)
  }

  def getAllFields(clazz: Class[_]): List[Field] =
    clazz.getDeclaredFields.toList ::: (clazz.getSuperclass match {
      case null => List();
      case c => getAllFields(c)
    })


  def getField[T: ClassTag](instance: Any, fieldName: String): Field = {
    val clazz = instance.getClass
    import scala.reflect.runtime.{universe => ru}
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val allFields = getAllFields(clazz)
    val field = allFields.filter(_.getName == fieldName) match {
      case f :: _ => f
      case Nil => throw new NoSuchFieldException(s"Class is ${clazz} asked for field $fieldName, legal values are [${clazz.getDeclaredFields.mkString(", ")}]")
    }
    val askedForClass = implicitly[ClassTag[T]].runtimeClass
    if (askedForClass != field.getType) throw new scala.ClassCastException(s"Actual class ${field.getType}, asked for is ${askedForClass}")
    field.setAccessible(true) //A fairie may die every time this line is executed
    field
  }


  def instantiate[T](clazz: Class[T]): T = {
    import scala.reflect.runtime.{universe => ru}
    val rm = ru.runtimeMirror(clazz.getClassLoader())
    val declaredFields = clazz.getFields().toList
    val moduleField = declaredFields.find(field => field.getName() == "MODULE$")
    try {
      val obj = moduleField match {
        case Some(modField) => modField.get(clazz)
        case None => clazz.newInstance()
      }
      obj.asInstanceOf[T]
    } catch {
      case e: Throwable => throw new RuntimeException(s"Class: $clazz Field: $moduleField", e)
    }
  }


}