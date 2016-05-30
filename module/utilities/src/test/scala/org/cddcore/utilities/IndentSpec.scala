package org.cddcore.utilities


class IndentSpec extends CddSpec {

  "Indent" should "make it easy to indent and unindent" in {
    val indent = Indent("..")
    indent.toString shouldBe ""
    indent.indent
    indent.indent
    indent.toString shouldBe "...."
    indent.unindent
    indent.toString shouldBe ".."
    indent.unindent
    indent.toString shouldBe ""
    indent.unindent
    indent.toString shouldBe ""
  }
}
