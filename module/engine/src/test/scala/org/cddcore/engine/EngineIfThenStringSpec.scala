package org.cddcore.engine

import org.cddcore.engine.enginecomponents.Scenario
import org.cddcore.utilities.CddSpec


class EngineIfThenStringSpec extends CddSpec {

  import Scenario._

  "An engine with no scenarios" should "have a simple ifThenString" in {
    new Engine[Int, String] {
    }.ifThenString shouldBe "Engine()"
  }

  "An engine which is just a Simple Reason conclusion node" should "have a simple IfThenString" in {
    if (FutureWorkFlags.engineIfThenString) {
      new Engine[Int, String] {
        1 produces "result"
      }.ifThenString shouldBe "Engine(if(true)result)"
    }
  }

}
