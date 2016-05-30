package org.cddcore.cddunit

import org.cddcore.utilities.CddSpec
import org.junit.runner.notification.RunNotifier

class MultipleCddUnitSpec extends CddSpec {

  def run[X <: CddContinuousIntegrationTest](test: Class[X]) = {
    new CddRunner(test).getDescription
  }

  "Multiple CddContinuousIntegrationTests" should "be aggregated" in {
    run(classOf[ExampleJUnit])
    run(classOf[ExampleJUnit2])
    run(classOf[ExampleJUnit3])
    CddContinuousIntegrationTest.tests.map(_.getClass) shouldBe List(classOf[ExampleJUnit], classOf[ExampleJUnit2], classOf[ExampleJUnit3])
  }

  they should "produce a map that is the render of all the maps, grouped by the tests" in {

  }
}
