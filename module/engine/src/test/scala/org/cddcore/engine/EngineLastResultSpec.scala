/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.engine

import org.cddcore.enginecomponents.Scenario

class EngineLastResultSpec extends CddEngineSpec {

  "last" should "be the result of the previous scenario in the same use case" in {
    var remembered1 = ""
    var remembered77 = ""

    new Engine[Int, String] {
      useCase("1") {
        1 produces "1"
        remembered1 = last
      }
      useCase("1") {
        77 produces "77"
        remembered77 = last
      }
    }
    remembered1 shouldBe "1"
    remembered77 shouldBe "77"
  }

  it should "be usable in a scenario" in {
    var remembered: Option[Scenario[String, String]] = None
    new Engine[String, String] {
      useCase("1") {
        "0" produces "1"
        remembered = Some(last produces "2")
      }
    }
    remembered.map(_.situation) shouldBe Some("1")
  }

  it should "throw NoLastException if at the start of an engine" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      rememberedException = intercept[NoLastException](last)

    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if no result is available" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      1 produces something where (_ => true)
      rememberedException = intercept[NoLastException](last)
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if at the start of a use case" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      useCase("1") {
        rememberedException = intercept[NoLastException](last)
      }
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }
  it should "throw NoLastException if at the start of a newuse case" in {
    var rememberedException: Exception = new RuntimeException
    new Engine[Int, String] {
      useCase("0") {}
      useCase("1") {
        rememberedException = intercept[NoLastException](last)
      }
    }
    rememberedException.isInstanceOf[NoLastException] shouldBe true
  }


}
