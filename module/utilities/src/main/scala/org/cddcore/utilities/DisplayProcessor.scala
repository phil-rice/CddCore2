package org.cddcore.utilities

object DisplayProcessor {
  type DpFunction = PartialFunction[(DisplayProcessor, Any), String]

  implicit val defaultDisplayProcessor = SimpleDisplayProcessor(List(), List(), List())

  protected case class SimpleDisplayProcessor(htmlers: List[DpFunction], summarizers: List[DpFunction], detailers: List[DpFunction]) extends DisplayProcessor {
    def apply(list: List[DpFunction], value: Any): String = {
      val tuple = (this, value)
      list.find(_.isDefinedAt(tuple)) match {
        case Some(f) => f(tuple)
        case _ => value.toString
      }
    }

    def html(x: Any) = x match {
      case h: ToHtml => h.toHtml(this)
      case _ => apply(htmlers, x)
    }

    def summarize(x: Any): String = x match {
      case h: ToSummary => h.toSummary(this)
      case _ => apply(summarizers, x)
    }

    def detailed(x: Any): String = x match {
      case h: ToDetailed => h.toDetailed(this)
      case _ => apply(detailers, x)
    }


    def withHtml(htmler: DpFunction) = copy(htmlers = htmler :: htmlers)

    def withSummarize(summarizer: DpFunction) = copy(summarizers = summarizer :: summarizers)

    def withDetailer(detailer: DpFunction) = copy(detailers = detailer :: detailers)
  }

}

trait DisplayProcessor {
  def html(x: Any): String

  def summarize(x: Any): String

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
