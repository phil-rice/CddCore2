package org.cddcore.utilities


object Strings {
  def cleanString(s: String, acceptedChars: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ") = s.filter(acceptedChars.contains(_)).mkString

  def cleanStringForJunitName(s: String) = cleanString(s, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_- ;.():")

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
    s1.zipAll(s2, null, null).zipWithIndex.foldLeft (start) {
      case (acc: String, ((ch1: Char, ch2: Char), i: Int)) =>
        if (ch1 != ch2) acc + s"Index $i ${ch1.toInt} ${ch2.toInt}\n" else acc
    }

  }

  //  def htmlEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br />")
  //
  //  def htmlTooltipEscape(raw: String) = raw.replace("&", "&amp;").replace("\"", "&quot;").replace("\'", "&apos;").replace("<", "&lt;").replace("&gt;", ">")

}
