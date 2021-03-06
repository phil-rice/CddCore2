/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.utilities


object Strings {
  def splitLines(s: String): Seq[String] = s.split("""(\r|\n|\f)""").filter(_.length>0).toSeq

  def trimChar(trim: Char)(s: String) = s.dropWhile(_ == trim).reverse.dropWhile(_ == trim).reverse


  def cleanString(s: String, acceptedChars: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ") = s.filter(acceptedChars.contains(_)).mkString

  def uri(parts: String*): String = parts.map(trimChar('/')).mkString("/")

  def cleanAsUri(parts: String*) = parts.map(cleanString(_)).mkString("/")


  def cleanStringForJunitName(s: String) = cleanString(s.replace(',', ' '), "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ;.():")

  def findFirstIncludingPrefix(prefix: String, postfix: String)(s: String) = {
    val firstIndex = s.indexOf(prefix)
    if (firstIndex == -1)
      None
    else {
      val start = firstIndex + prefix.length
      val secondIndex = s.indexOf(postfix, start)
      if (secondIndex == -1)
        None
      else
        Some(s.substring(firstIndex, secondIndex).trim)
    }
  }

  def findFirst(prefix: String, postfix: String)(s: String) = {
    val firstIndex = s.indexOf(prefix)
    if (firstIndex == -1)
      None
    else {
      val start = firstIndex + prefix.length
      val secondIndex = s.indexOf(postfix, start)
      if (secondIndex == -1)
        None
      else
        Some(s.substring(start, secondIndex).trim)
    }
  }

  def from(prefix: String)(s: String) = {
    val firstIndex = s.indexOf(prefix)
    if (firstIndex == -1)
      None
    else
      Some(s.substring(firstIndex))
  }

  def removeLast(count: Int)(s: String) = {
    val i = s.length - count
    s.substring(0, i)
  }

  def oneLine(s: Any) = s.toString.replace('\n', ' ')

  def bruteForceCompare(s1: String, s2: String) = {
    val start = s"lengths(${s1.size}, ${s2.size}\n"
    s1.zipAll(s2, null, null).zipWithIndex.foldLeft(start) {
      case (acc: String, ((ch1: Char, ch2: Char), i: Int)) =>
        if (ch1 != ch2) acc + s"Index $i ${ch1.toInt} ${ch2.toInt}\n" else acc
    }
  }

  //  def htmlEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br />")
  //
  //  def htmlTooltipEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace("&gt;", ">")

}
