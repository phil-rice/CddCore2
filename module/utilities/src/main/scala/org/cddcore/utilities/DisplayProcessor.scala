package org.cddcore.utilities

object DisplayProcessor {
  type DpFunction = PartialFunction[(DisplayProcessor, Any), String]

  implicit val defaultDisplayProcessor = SimpleDisplayProcessor(List(), List(), List())

  protected case class SimpleDisplayProcessor(htmlers: List[DpFunction], summarizers: List[DpFunction], detailers: List[DpFunction]) extends DisplayProcessor {
    def applyFn(list: List[DpFunction], value: Any, fn: Any => String): String = {
      val tuple = (this, value)
      list.find(_.isDefinedAt(tuple)) match {
        case Some(f) => f(tuple)
        case _ => value match {
          case (a, b) => s"(${fn(a)},${fn(b)})"
          case (a, b, c) => s"(${fn(a)},${fn(b)},${fn(c)})"
          case m: Map[_, _] => m.map {
            case kv@(k, v) =>
              list.find(_.isDefinedAt(this, kv)) match {
                case Some(f) => f(this, kv)
                case _ => s"${fn(k)} -> ${fn(v)}"
              }
          }.mkString("Map(", ",", ")")
          case l: List[_] => l.map(fn).mkString("List(", ",", ")")
          case _ => value.toString
        }
      }
    }

    def html(x: Any): String = x match {
      case h: ToHtml => h.toHtml(this)
      case _ => applyFn(htmlers, x, html(_))
    }

    def apply(x: Any): String = x match {
      case h: ToSummary => h.toSummary(this)
      case _ => applyFn(summarizers, x, apply(_))
    }

    def detailed(x: Any): String = x match {
      case h: ToDetailed => h.toDetailed(this)
      case _ => applyFn(detailers, x, detailed(_))
    }

    def withHtml(htmler: DpFunction) = copy(htmlers = htmler :: htmlers)

    def withSummarize(summarizer: DpFunction) = copy(summarizers = summarizer :: summarizers)

    def withDetailer(detailer: DpFunction) = copy(detailers = detailer :: detailers)
  }

}

trait DisplayProcessor {
  def html(x: Any): String

  def apply(x: Any): String

  def detailed(x: Any): String
}


trait ToHtml {
  def toHtml(displayProcessor: DisplayProcessor): String
}

trait ToDetailed {
  def toDetailed(displayProcessor: DisplayProcessor): String
}

trait ToSummary {
  def toSummary(displayProcessor: DisplayProcessor): String
}
