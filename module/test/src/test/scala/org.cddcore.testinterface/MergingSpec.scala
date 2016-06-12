package org.cddcore.testinterface

import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.engine.Engine
import org.junit.runner.RunWith

@RunWith(classOf[CddRunner])
class MergingSpec extends CddContinuousIntegrationTest{
  override def engines: List[Engine[_, _]] = List( DemonstrationOfMerging.engine)
}
