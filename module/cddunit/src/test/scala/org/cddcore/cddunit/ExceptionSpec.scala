package org.cddcore.cddunit

import org.cddcore.utilities.CddSpec

class ExceptionSpec extends CddSpec {

  "The exceptions reported by the CddRunner" should "have the first line of the stack track being the 'situation declared at' line" in {
    fail
  }
}
