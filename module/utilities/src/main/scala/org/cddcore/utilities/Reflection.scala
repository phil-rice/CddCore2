package org.cddcore.utilities

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, InvocationTargetException, Method}

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object Reflection {

  implicit def toFieldMapPimper[V](fieldMap: Map[Field, Try[V]]) = new FieldMapPimper(fieldMap)

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
      case e: Throwable => throw new RuntimeException(s"Instantiating class: $clazz moduleField (i.e. 'is an object not a class'): $moduleField", e)
    }
  }

}

class FieldMapPimper[V](fieldMap: Map[Field, Try[V]]) {
  def sorted(allFields: List[Field]) = ListMap[Field, Try[V]](fieldMap.toList.sortBy { case (f, _) => allFields.indexOf(f) }: _*)

  def displayStringMap(valueFn: V => String = (v: V) => Strings.oneLine(v)): Map[Field, Try[String]] = {
    fieldMap.map { case (f, tryV) =>
      val s = tryV.transform(x => Try(valueFn(x)), e => Success(s"<Error>${e.getClass.getSimpleName}/${e.getMessage}</error>"))
      (f, s)
    }
  }

  def displayString(separator: String = "\r\n") = fieldMap.toList.map{case (f, v) => f.getName +" -> " +v.get}.mkString(separator) //yes this throws the exception if v has one. if you don't want that, don't have an exception here

  def removeAnnotationFromFieldMap[A <: Annotation : ClassTag]: Map[Field, Try[V]] = {
    val aClass = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
    fieldMap.filter { case (f, tryV) => f.getAnnotation(aClass) == null }
  }
}


class Reflection(instance: Any) {

  import Reflection._

  //  val instanceClass = instance.getClass
  lazy val allFields = getAllFields(instance.getClass)
  lazy val allMethods = getAllMethods(instance.getClass)

  def instantiateIfLazyVal(field: Field) = {
    val methods = allMethods.
      filter(m => (m.getParameterTypes().length == 0) && field.getType == m.getReturnType && field.getName == m.getName)
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


  def getFieldValue[T: ClassTag](fieldName: String): Try[T] = {
    val field: Field = getField(fieldName)
    getFieldValue(field)
  }

  def getFieldValue[T](field: Field): Try[T] = try {
    instantiateIfLazyVal(field)
    field.setAccessible(true)
    Success(field.get(instance).asInstanceOf[T])
  } catch {
    case e: InvocationTargetException => Failure(e.getCause)
    case e: Exception => Failure(e)
  }

  def modField[T: ClassTag](fieldName: String)(fn: T => T) = {
    val field = getField[T](fieldName)
    val oldValue = field.get(instance).asInstanceOf[T]
    val newValue = fn(oldValue)
    field.set(instance, newValue)
  }

  def fieldMap[T: ClassTag]: Map[Field, Try[T]] = {
    val returnType = implicitly[ClassTag[T]].runtimeClass
    Map[Field, Try[T]]() ++ allFields.
      filter(f => returnType.isAssignableFrom(f.getType)).
      map((f) => (f -> getFieldValue[T](f)))
  }


  def fieldMapForAnnotation[A <: Annotation : ClassTag]: Map[Field, Try[Any]] = {
    val annotationType: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
    Map[Field, Try[Any]]() ++ allFields.
      filter(f => f.getAnnotation(annotationType) != null).
      map((f) => (f -> getFieldValue[Any](f)))
  }

  //  def fieldMapToString[V](fieldMap: Map[Field, Try[V]], valueFn: V => String = (v: V) => Strings.oneLine(v), separator: String = "\r\n") =
  //    ListMap[Field, Try[V]](fieldMap.toList.sortBy { case (field, _) => allFields.indexOf(field) }: _*).map {
  //      case (f, v) =>
  //        val s = v.transform(x => Try(valueFn(x)), e => Success(s"<Error>${e.getClass.getSimpleName}/${e.getMessage}</error>")).get
  //        (f.getName, s)
  //    }.mkString(separator)
  //
  //  def removeAnnotationFromFieldMap[A <: Annotation : ClassTag](fieldMap: Map[Field, X])


}
