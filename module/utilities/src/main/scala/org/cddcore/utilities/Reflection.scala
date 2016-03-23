package org.cddcore.utilities

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, Method}

import scala.reflect.ClassTag

object Reflection {

  def apply(instance: Any) = new Reflection(instance)

  def getAllFields(clazz: Class[_]): List[Field] =
    clazz.getDeclaredFields.toList ::: (clazz.getSuperclass match {
      case null => List();
      case c => getAllFields(c)
    })

  def getAllMethods(clazz: Class[_]): List[Method] =
    clazz.getDeclaredMethods.toList ::: (clazz.getSuperclass match {
      case null => List();
      case c => getAllMethods(c)
    })


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


class Reflection(instance: Any) {

  import Reflection._

  //  val instanceClass = instance.getClass
  lazy val allFields = getAllFields(instance.getClass)
  lazy val allMethods = getAllMethods(instance.getClass)

  def instantiateIfLazyVal(field: Field) = {
    val methods = allMethods.
      filter(m => (m.getParameterTypes().length == 0) && field.getType == m.getReturnType)
    for (m <- methods) {
      m.setAccessible(true)
      m.invoke(instance)
    }
  }

  def getField[T: ClassTag](fieldName: String): Field = {
    val clazz = instance.getClass
    import scala.reflect.runtime.{universe => ru}
    val rm = ru.runtimeMirror(clazz.getClassLoader())

    val field = allFields.filter(_.getName == fieldName) match {
      case f :: _ => f
      case Nil => throw new NoSuchFieldException(s"Class is ${clazz} asked for field $fieldName, legal values are [${clazz.getDeclaredFields.mkString(", ")}]")
    }
    val askedForClass = implicitly[ClassTag[T]].runtimeClass
    if (askedForClass != field.getType) throw new scala.ClassCastException(s"Actual class ${field.getType}, asked for is ${askedForClass}")
    field.setAccessible(true) //A fairie may die every time this line is executed
    field
  }


  def getFieldValue[T: ClassTag](fieldName: String): T = {
    val field: Field = getField(fieldName)
    getFieldValue(field)
  }

  def getFieldValue[T](field: Field): T = {
    instantiateIfLazyVal(field)
    field.setAccessible(true)
    field.get(instance).asInstanceOf[T]
  }

  def modField[T: ClassTag](fieldName: String)(fn: T => T) = {
    val field = getField[T](fieldName)
    val oldValue = field.get(instance).asInstanceOf[T]
    val newValue = fn(oldValue)
    field.set(instance, newValue)
  }

  def fieldMap[T: ClassTag]: Map[Field, T] = {
    val returnType = implicitly[ClassTag[T]].runtimeClass
    Map[Field, T]() ++ allFields.
      filter(f => returnType.isAssignableFrom(f.getType)).
      map((f) => (f -> getFieldValue[T](f)))
  }

  def fieldMapForAnnotation[A <: Annotation : ClassTag]: Map[Field, Any] = {
    val annotationType: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
    Map[Field, Any]() ++ allFields.
      filter(f => f.getAnnotation(annotationType) != null).
      map((f) => (f -> getFieldValue[Any](f)))
  }

  def fieldMapToString[V](fieldMap: Map[Field, V], valueFn: V => String = (v: V) => Strings.oneLine(v), separator: String = "\r\n") =
    fieldMap.map { case (f, v) => (f.getName, valueFn(v)) }.mkString(separator)


}
