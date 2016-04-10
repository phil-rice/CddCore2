/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
