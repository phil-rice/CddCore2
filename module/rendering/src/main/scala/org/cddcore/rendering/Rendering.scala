package org.cddcore.rendering


object Rendering {

  import mustache._

  def main(args: Array[String]) {
    val template = new Mustache("Hello, {{ name }}!")
    println(template.render(Map("name" -> "world")))
    println("Hello World")
  }

}
