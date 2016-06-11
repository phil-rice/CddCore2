/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.{RememberingLifeCycle, Scenario}


class ConflictingScenariosSpec extends CddNonRecursiveSpec[String, String] {

  import Scenario._

  implicit val lifeCycle = new RememberingLifeCycle[String, String]

  "Two conflicting scenarios without whys" should "report an error" in {
    val scenario1 = "situation1" produces "result1"
    val scenario2 = "situation2" produces "result2"
    lifeCycle.errors shouldBe List()
    DecisionTree(mockEngine, Seq(scenario1, scenario2))
    val Seq(onlyError) = lifeCycle.errors
    onlyError should include("""CannotAddScenarioException/Scenario defined at (ConflictingScenariosSpec.scala:15) conflicts with (ConflictingScenariosSpec.scala:14)""")
    onlyError should include("""Scenario being added is (ConflictingScenariosSpec.scala:15) situation2 produces result2)""")
    onlyError should include("""Scenario already existing is (ConflictingScenariosSpec.scala:14) situation1 produces result1)""")
    onlyError should include("""If it was added, would come to result""")
    onlyError should include("""result1""")
  }

  "A decistion tree with different simple reasons coming to the same conclusions, two of them with a by" should "report an error" in {
    if (FutureWorkFlags.noticingScenariosWithBy) fail
  }


}
