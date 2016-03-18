package org.cddcore.utilities

trait HierarchyTestFramework {

  class Thing(val name: String) {
    override def equals(other: Any) = other match {
      case t: Thing => t.name == name && t.getClass == getClass;
      case _ => false
    }

    override def hashCode = name.hashCode

    override def toString = s"Thing($name)"
  }

  case class ThingHolder(override val name: String, list: List[Thing], errors: Map[Thing, Throwable]) extends Thing(name) {
    def withErrors(errors: Map[Thing, Throwable]) = copy(errors = errors)

    override def equals(other: Any) = other match {
      case t: ThingHolder => t.name == name && t.getClass == getClass && t.list == list && t.errors == errors
      case _ => false
    }
    override def toString = s"ThingHolder($name,$list,$errors)"

  }

  implicit object ThingHierarcy extends Hierarchy[ThingHolder, Thing] {
    def withNewChild(h: ThingHolder, child: Thing): ThingHolder = h.copy(list = child :: h.list)

    def lastAddedChild(h: ThingHolder) = h.list.headOption

    def childToHolder(child: Thing): ThingHolder = child.asInstanceOf[ThingHolder]

    def modChild(h: ThingHolder, fn: (Thing) => Thing): ThingHolder = h.list match {
      case (oldHead :: tail) => h.copy(list = fn(oldHead) :: tail)
    }

    def badChild(topParent: ThingHolder, child: Thing, exception: Exception): ThingHolder =
      topParent.withErrors(topParent.errors + (child -> exception))
  }

  def holder(s: String, children: Thing*) = ThingHolder(s, children.toList, Map())

  val holder1 = holder("useCase1")
  val holder2 = holder("useCase2")
  val holder3 = holder("useCase3")
  val holder4 = holder("useCase4")

  val t1 = new Thing("1")
  val t2 = new Thing("2")
  val t3 = new Thing("3")
}

class HierarchySpec extends CddSpec with HierarchyTestFramework {


  "A HierarcyBuilder with no operations" should "have the passed in  holder and depth 0" in {
    val builder = new HierarchyBuilder[ThingHolder, Thing](holder1)
    builder.holder shouldBe holder1
    builder.depth shouldBe 0
    builder.currentChild shouldBe None
    builder.currentParent shouldBe holder1
  }

  "A HierarcyBuilder addChild method with depth 0" should "add children to the holder and not mess with depth" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addChild(t1).addChild(t2).addChild(t3)
    builder2.holder shouldBe holder1.copy(list = List(t3, t2, t1))
    builder2.depth shouldBe 0
    builder2.currentParent shouldBe builder2.holder
    builder2.currentChild shouldBe Some(t3)
  }
  "A HierarcyBuilder addNewParent method " should "nest children with new holders increasing depth" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3)
    builder2.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3")))
    builder2.depth shouldBe 2
    builder2.currentParent shouldBe holder("useCase3")
    builder2.currentChild shouldBe None
  }
  it should "allow scenarios to be added to current use case" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1).addChild(t2).addChild(t3)
    builder2.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3", t3, t2, t1)))
    builder2.depth shouldBe 2
    builder2.currentParent shouldBe holder("useCase3", t3, t2, t1)
    builder2.currentChild shouldBe Some(t3)
  }
  it should "allow scenarios to be added to current holders, then a pop and another use case added" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)

    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1).popParent
    builder2.depth shouldBe 1
    val builder3 = builder2.addNewParent(holder4).addChild(t2).addChild(t3)
    builder3.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase4", t3, t2), holder("useCase3", t1)))
    builder3.depth shouldBe 2
    builder3.currentParent shouldBe holder("useCase4", t3, t2)
    builder3.currentChild shouldBe Some(t3)
  }

  it should "Keep track of children that threw exceptions while transforming. Note that this behaviour is controlled by the Hierarchy type class" in {
    val e1 = new RuntimeException("Exception 1")
    val e2 = new RuntimeException("Exception 2")
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1)
    val builder3 = builder2.modCurrentChild(_ => throw e1).addChild(t2).modCurrentChild(_ => throw e2).addChild(t3)
    builder3.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3", t3, t2, t1))).withErrors(Map(t1 -> e1, t2 -> e2))
  }

  it should "Allow exceptions to be added against children whether or not they are the current child" in {
    val e2 = new RuntimeException("Exception 2")
    val e3 = new RuntimeException("Exception 3")
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1)
    val builder3 = builder2.addChild(t2).addChild(t3)
    val builder4 = builder3.childHasException(t2, e2).childHasException(t3, e3)
    builder4.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3", t3, t2, t1))).withErrors(Map(t2 -> e2, t3 -> e3))

  }

}
