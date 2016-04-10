/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.utilities

class MapsSpec extends CddSpec {

  import Maps._

  val ma1 = Map("a" -> 1)
  val mb2 = Map("b" -> 2)
  val mc3 = Map("c" -> 3)
  val ma1b2 = ma1 ++ mb2
  val ma1c3 = ma1 ++ mc3

  "The Maps pimper" should "recursivelyNotFilter" in {
    val m = ma1 ++ Map("x" -> ma1b2, "y" -> ma1c3)
    m.recursivelyNotFilter({ case (k, v) => k == "a" }) shouldBe Map("x" -> mb2, "y" -> mc3)
  }
  it should "recursivelyFilter" in {
    val m = Map("a" -> 1, "x" -> ma1b2, "y" -> ma1c3)
    m.recursivelyFilter({ case (k: String, v) => k == "a" }) shouldBe ma1 ++ Map("x" -> ma1, "y" -> ma1)
  }
  it should "recursivelyTransform" in {
    val m = Map("a" -> 1, "x" -> ma1b2, "y" -> ma1c3)
    m.recursivelyTransform { case (k: String, v: Int) if (k == "a") => ("q", v + 1) } shouldBe  Map("q" -> 2, "x" -> Map("q" -> 2, "b" -> 2), "y" -> Map("q" -> 2, "c" -> 3))
  }

}

class MapOfListsSpec extends CddSpec{
  import MapOfLists._

  "The map of lists " should "allow items to be added to the list represented by a key" in {
    Map[String, List[Int]]().addToList("a" -> 1).addToList("a" -> 2).addToList("b" -> 3) shouldBe Map("a" -> List(1,2), "b" -> List(3))
  }
}
