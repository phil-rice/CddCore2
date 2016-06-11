package org.cddcore.testinterface

import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner, ExampleJUnit}
import org.cddcore.engine.Engine
import org.cddcore.examples.{Bowling, ProcessChequeXml, Tennis}
import org.junit.runner.RunWith


@RunWith(classOf[CddRunner])
class Sample extends CddContinuousIntegrationTest {
  override def engines: List[Engine[_, _]] = List(Tennis.tennis)
}

@RunWith(classOf[CddRunner])
class Sample2 extends CddContinuousIntegrationTest {
  override def engines: List[Engine[_, _]] = List(Bowling.get, Bowling.makeFrame)
}

//@RunWith(classOf[CddRunner]) //un comment to see the errors in the report
//class SampleWithErrors extends CddContinuousIntegrationTest {
//  override def engines: List[Engine[_, _]] = List(ExampleJUnit.engine1, ProcessChequeXml.processCheque)
//}
