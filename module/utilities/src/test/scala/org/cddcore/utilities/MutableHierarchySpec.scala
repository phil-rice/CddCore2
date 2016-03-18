package org.cddcore.utilities


class HierarchyTestFrameworkSpec extends CddSpec with HierarchyTestFramework{

  "The thing equals method" should "work" in {
    new Thing("123") shouldBe new Thing("123")
    new Thing("123") shouldNot be (new Thing("abc"))
    new Thing("123") shouldNot be (null)
  }

  "The thing holder equals method"should "work " in {
    holder("name1", new Thing("123")) shouldBe holder("name1", new Thing("123"))
    holder("name1",   new Thing("123")) shouldNot be (  holder("name1", new Thing("abc")))
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
    hierachyBuilder.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase4", t3, t2), holder("useCase3", t1)))
  }

  it should "record changes to the held children" in {
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    modCurrentChild(_ => t2)
    hierachyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t2))
  }

  it should "handle exceptions during transforms" in {
    val e = new RuntimeException("Exception1")
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    modCurrentChild(_ => throw e)
    hierachyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t1)).withErrors(Map(t1 -> e))
  }

  it should "allow children to be marked as a problem with an exception" in {
    val e = new RuntimeException("Exception1")
    val builder = new SimpleMutableHierarchyBuilder[ThingHolder, Thing](holder1)
    import builder._
    addNewParent(holder2)
    addChild(t1)
    childHasException(t1, e)
    hierachyBuilder.holder shouldBe holder("useCase1", holder("useCase2", t1)).withErrors(Map(t1 -> e))

  }

}
