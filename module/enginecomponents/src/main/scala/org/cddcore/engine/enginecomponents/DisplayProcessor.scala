package org.cddcore.engine.enginecomponents

object DisplayProcessor {
  implicit val defaultDisplayProcessor = DisplayProcessor({ case x => x.toString }, { case x => x.toString }, { case x => x.toString })
}

case class DisplayProcessor(html: PartialFunction[Any, String], summarize: PartialFunction[Any, String], detailed: PartialFunction[Any, String]) {
  def withHtml(html: PartialFunction[Any, String]) = copy(html = html andThen this.html)

  def withSummarize(summarize: PartialFunction[Any, String]) = copy(summarize = summarize andThen this.summarize)

  def withDetailed(detailed: PartialFunction[Any, String]) = copy(detailed = detailed andThen this.detailed)
}
