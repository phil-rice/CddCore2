/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
  override def engines: List[Engine[_, _]] = List(Bowling.get, Bowling.makeFrame, ProcessChequeXml.processCheque)
}
//
//@RunWith(classOf[CddRunner]) //un comment to see the errors in the report
//class SampleWithErrors extends CddContinuousIntegrationTest {
//  override def engines: List[Engine[_, _]] = List(ExampleJUnit.engine1,
//    ExampleJUnit.invalidScenarios,
//    ExampleJUnit.malformedScenarios,
//    ExampleJUnit.redundantNeither,
//    ExampleJUnit.redundantMain,
//    ExampleJUnit.redundantS,
//    ExampleJUnit.conflictingNoReasons,
//    ExampleJUnit.conflictingNewHasReasons,
//    ExampleJUnit.conflictingMainHasReasons,
//    ExampleJUnit.conflictingBothReasons
//  )
//}
