package org.cddcore.rendering

import java.io.{StringReader, StringWriter}
import java.util

import com.github.mustachejava.{DefaultMustacheFactory, Mustache => JMustache}


object Mustache {
  val mf = new DefaultMustacheFactory();

  def apply(name: String, template: String) = new Mustache(mf.compile(new StringReader(template), name))

  def apply(name: String) = new Mustache(mf.compile(name))


  def forMustache(s: Any): Any = s match {
    case m: Map[_, _] => m.foldLeft(new util.HashMap[String, Any]) {
      case (acc, (k: String, v)) => acc.put(k, forMustache(v))
        acc
    }
    case l: List[_] => l.foldLeft(new util.ArrayList[Any]) {
      (acc, v) => acc.add(forMustache(v))
        acc
    }
    case _ => s
  }
}

class Mustache(mustache: JMustache) {
  def apply(item: Any) = {
    val writer = new StringWriter()
    mustache.execute(writer, Mustache.forMustache(item))
    writer.flush()
    writer.toString
  }
}
