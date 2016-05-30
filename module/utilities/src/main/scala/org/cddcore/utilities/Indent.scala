package org.cddcore.utilities

object Indent {
  def apply(each: String = "  ") = new Indent(each)
}

class Indent(each: String) {
  var prefix: String = ""

  def indent = prefix = prefix + each

  def unindent = prefix = prefix.dropRight(each.length)

  override def toString: String = prefix
}
