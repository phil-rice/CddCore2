package org.cddcore.utilities

import org.cddcore.utilities.Strings

class StringsSpec extends CddSpec {

  "The Strings.findFirst" should "extract the first chunk that matches the criteria " in {
    Strings.findFirst("from", "to")("fromheretohere") shouldBe Some("here")
    Strings.findFirst("from", "to")("asdfromheretohere") shouldBe Some("here")
    Strings.findFirst("from", "to")("sdfromheretofromhere") shouldBe Some("here")
    Strings.findFirst("from", "to")("") shouldBe None
    Strings.findFirst("", "")("asd") shouldBe Some("")
    Strings.findFirst("", "a")("asd") shouldBe Some("")
  }

  "The Strings.findFirstIncludingPrefix" should "extract the first chunk that matches the criteria " in {
    Strings.findFirstIncludingPrefix("from", "to")("fromheretohere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("fromheretofromhere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("asdfromheretohere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("sdffromheretofromhere") shouldBe Some("fromhere")
    Strings.findFirstIncludingPrefix("from", "to")("") shouldBe None
    Strings.findFirstIncludingPrefix("", "")("asd") shouldBe Some("")
    Strings.findFirstIncludingPrefix("", "a")("asd") shouldBe Some("")
  }

  "The Strings.from" should "extract the first chunk that matches the criteria " in {
    Strings.from("from")("fromheretohere") shouldBe Some("fromheretohere")
    Strings.from("from")("fromheretofromhere") shouldBe Some("fromheretofromhere")
    Strings.from("from")("asdfromheretohere") shouldBe Some("fromheretohere")
    Strings.from("from")("sdffromheretofromhere") shouldBe Some("fromheretofromhere")
    Strings.from("from")("") shouldBe None
    Strings.from("")("asd") shouldBe Some("asd")
    Strings.from("")("asd") shouldBe Some("asd")
  }

  "The Strings.removeLast" should "remove the last few characters from a string" in {
    Strings.removeLast(0)("asdasd") shouldBe "asdasd"
    Strings.removeLast(2)("asdasd") shouldBe "asda"
  }
}
