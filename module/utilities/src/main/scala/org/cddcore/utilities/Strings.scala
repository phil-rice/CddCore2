package org.cddcore.utilities


object Strings {
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


}
