/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.utilities

class ThingDslSpec extends CddSpec with HierarchyTestFramework {

  class ThingHolderDSL(val makeRootHolder: ThingHolder, val postSealMessage: String = "Cannot change after seal")(implicit val hierarchy: Hierarchy[ThingHolder, Thing]) extends MutableHierarchyBuilderWithChildLifeCycle[ThingHolder, Thing] {

    implicit def thingToPimper(thing: Thing) = new ThingPimper(thing)

    class ThingPimper(thing: Thing)(implicit childLifeCycle: ChildLifeCycle[Thing]) {
      def mod(fn: String => String) = childLifeCycle.update(new Thing(fn(thing.name)))


      def upperString = mod(_.toUpperCase)
    }


    def t(name: String) = {
      val result = new Thing(name)
      childLifeCycle.created(result)
      result
    }

    def thingHolder(name: String)(block: => Unit) = addParentChildrenDefinedInBlock(new ThingHolder(name, List(), Map()))(block)
  }

  "The mutable hierarchy when implementing a DSL like the usecases in Engine" should "listen to the child lifecycle when objects are created" in {
    val builder = new ThingHolderDSL(holder1) {
      thingHolder("useCase2") {
        thingHolder("useCase3") {
          t("1")
        }
        thingHolder("useCase4") {
          t("2")
          t("3")
        }
      }
    }
    builder.hierarchyBuilder.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase4", t3, t2), holder("useCase3", t1)))
  }

  it should "allow changes of the current child" in {
    val builder = new ThingHolderDSL(holder1) {
      t("abc") upperString
    }
    builder.hierarchyBuilder.holder shouldBe holder("useCase1", new Thing("ABC"))
  }
  it should "record exceptions if modifying the current child throws an exception " in {
    val e1 = new RuntimeException("exception1")
    val builder = new ThingHolderDSL(holder1) {
      t("abc") mod (_ => throw e1)
    }
    builder.hierarchyBuilder.holder shouldBe holder("useCase1", new Thing("abc")).withErrors(Map(new Thing("abc") -> e1))

  }

}
