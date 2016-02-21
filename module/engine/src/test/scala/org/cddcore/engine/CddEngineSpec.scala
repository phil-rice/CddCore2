package org.cddcore.engine

import org.cddcore.utilities.CddSpec


trait CddEngineSpec extends CddSpec {

}

trait CddNonRecursiveSpec[P, R] extends CddEngineSpec {
  def mockEngine[P, R]: P => R = _ => throw new RuntimeException("Should not try anything recursive in this test")
}
