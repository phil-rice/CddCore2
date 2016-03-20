package org.cddcore.utilities

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.Context

object CodeHolder {
  implicit def fnToHolder[Fn](fn: Fn): CodeHolder[Fn] = macro fnToHolder_impl[Fn]

  def fnToHolder_impl[Fn: c.WeakTypeTag](c: Context)(fn: c.Expr[Fn]): c.Expr[CodeHolder[Fn]] = {
    import c.universe._
    reify {
      CodeHolder[Fn](fn.splice, c.literal(show(fn.tree)).splice, "")
    }
  }

  def partialFnToStringToNiceToString(s: String) = {
    val firstIndex = s.indexOf("case")
    if (firstIndex == -1)
      None
    else {
      val secondIndex = s.indexOf("case", firstIndex + 1)
      if (secondIndex == -1)
        None
      else
        Some("{"+s.substring(firstIndex, secondIndex).trim +"}")
    }
  }

}

case class CodeHolder[Fn](val fn: Fn, val description: String, val comment: String = "") extends AbstractCodeHolder {
  //Need this to allow tests to pass. Obviously this is dodgy if we start putting them in maps etc... so don't!
  override def equals(other: Any): Boolean = other match {
    case c: CodeHolder[Fn] => c.description == description
    case _ => false
  }

  def prettyDescription = description match {
    case d if description.contains(
      "SerialVersionUID(value = 0) final <synthetic> class $anonfun extends scala.runtime.AbstractPartialFunction[") => CodeHolder.partialFnToStringToNiceToString(d).getOrElse(d)
    case d if description.startsWith("((") && description.endsWith("})") && description.contains("match") => {
      Strings.from("case")(d).map(Strings.removeLast(2)).map(_.trim).getOrElse(d)
    }
    case d => d
  }

  override def hashCode() = description.hashCode()
}

trait AbstractCodeHolder {
  def description: String

  def comment: String

  private val index = description.indexOf("=>");
//  lazy val pretty = (index match {
//    case -1 => description
//    case i => description.substring(index + 3, description.length - 1)
//  }).replace(".this.", ".").replace(".apply(", "(")

  val parameters = index match {
    case -1 => description
    case i => description.substring(0, index);
  }

  override def toString = getClass.getSimpleName() + "(" + description + ")"
}

