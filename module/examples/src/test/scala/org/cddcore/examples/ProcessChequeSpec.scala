package org.cddcore.examples

import org.cddcore.cddunit.{CddContinuousIntegrationTest, CddRunner}
import org.cddcore.engine.Engine
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.junit.runner.RunWith


@RunWith(classOf[CddRunner])
class ProcessChequeSpec extends CddContinuousIntegrationTest {
  val engines: List[Engine[_, _]] = List(ProcessChequeXml.processCheque)
}
