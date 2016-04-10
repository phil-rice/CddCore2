/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.utilities


class HierarchyTestFrameworkSpec extends CddSpec with HierarchyTestFramework {

  "The thing equals method" should "work" in {
    new Thing("123") shouldBe new Thing("123")
    new Thing("123") shouldNot be(new Thing("abc"))
    new Thing("123") shouldNot be(null)
  }

  "The thing holder equals method" should "work " in {
    holder("name1", new Thing("123")) shouldBe holder("name1", new Thing("123"))
    holder("name1", new Thing("123")) shouldNot be(holder("name1", new Thing("abc")))
  }
}

class MutableHierarchySpec extends CddSpec with HierarchyTestFramework {

  "The mutable hierarchy " should "allow parents to be added" in {
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._

    addNewParent(holder2)
    addNewParent(holder3)
    addChild(t1)
    popParent
    addNewParent(holder4)
    addChild(t2)
    addChild(t3)
    hierarchyBuilder.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase4", t3, t2), holder("useCase3", t1)))
  }

  it should "record changes to the held children" in {
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    modCurrentChild(_ => t2)
    hierarchyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t2))
  }

  it should "handle exceptions during transforms" in {
    val e = new RuntimeException("Exception1")
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    modCurrentChild(_ => throw e)
    hierarchyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t1)).withErrors(Map(t1 -> e))
  }

  it should "allow children to be marked as a problem with an exception" in {
    val e = new RuntimeException("Exception1")
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    childHasException(t1, e)
    hierarchyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t1)).withErrors(Map(t1 -> e))
  }

  it should "throw exceptions if modification methods are called after seal" in {
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1, "somePostSealMessage")
    import builder._
    def check(clue: String, block: => Unit) = withClue(clue)(intercept[CannotAddItemsException](block).getMessage shouldBe "somePostSealMessage")
    seal
    check("addChild", addChild(holder2))
    check("addNewParent", addNewParent(holder2))
    check("popParent", popParent)
    check("getCurrentChild", getCurrentChild)
    check("modCurrentChild", modCurrentChild(a => a))
    check("childHasException", childHasException(holder2,  new RuntimeException("Exception1")))
  }

}
