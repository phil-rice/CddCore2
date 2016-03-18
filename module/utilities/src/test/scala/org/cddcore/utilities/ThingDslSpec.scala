package org.cddcore.utilities

class ThingDslSpec extends CddSpec with HierarchyTestFramework {

  class ThingHolderDSL(val makeRootHolder: ThingHolder)(implicit val hierarchy: Hierarchy[ThingHolder, Thing]) extends MutableHierarchyBuilderWithChildLifeCycle[ThingHolder, Thing] {

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
