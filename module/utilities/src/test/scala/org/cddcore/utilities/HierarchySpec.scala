package org.cddcore.utilities


class HierarchySpec extends CddSpec {

  class Thing(name: String) {
    override def toString = s"Thing($name)"
  }

  case class ThingHolder(name: String, list: List[Thing]) extends Thing(name)


  implicit object ThingHierarcy extends Hierarchy[ThingHolder, Thing] {
    def withNewChild(h: ThingHolder, child: Thing): ThingHolder = h.copy(list = child :: h.list)

    def currentChild(h: ThingHolder) = h.list.headOption

    def childToHolder(child: Thing): ThingHolder = child.asInstanceOf[ThingHolder]

    def modChild(h: ThingHolder, fn: (Thing) => Thing): ThingHolder = h.list match {
      case (oldHead :: tail) => h.copy(list = fn(oldHead) :: tail)
    }
  }

  def holder(s: String, children: Thing*) = ThingHolder(s, children.toList)

  val holder1 = holder("useCase1")
  val holder2 = holder("useCase2")
  val holder3 = holder("useCase3")
  val holder4 = holder("useCase4")

  val t1 = new Thing("1")
  val t2 = new Thing("2")
  val t3 = new Thing("3")


  "A HierarcyBuilder with no operations" should "have the passed in  holder and depth 0" in {
    val builder = new HierarchyBuilder[ThingHolder, Thing](holder1)
    builder.holder shouldBe holder1
    builder.depth shouldBe 0
    builder.getCurrentChild shouldBe None
  }

  "A HierarcyBuilder addChild method with depth 0" should "add children to the holder and not mess with depth" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addChild(t1).addChild(t2).addChild(t3)
    builder2.holder shouldBe holder1.copy(list = List(t3, t2, t1))
    builder2.depth shouldBe 0
    builder2.getCurrentChild shouldBe Some(t3)
  }
  "A HierarcyBuilder addNewParent method " should "nest children with new holders increasing depth" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3)
    builder2.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3")))
    builder2.depth shouldBe 2
    builder2.getCurrentChild shouldBe None
  }
  it should "allow scenarios to be added to current use case" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)
    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1).addChild(t2).addChild(t3)
    builder2.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase3", t3, t2, t1)))
    builder2.depth shouldBe 2
    builder2.getCurrentChild shouldBe Some(t3)
  }
  it should "allow scenarios to be added to current uholders, then a pop and another use case added" in {
    val builder1 = new HierarchyBuilder[ThingHolder, Thing](holder1)

    val builder2 = builder1.addNewParent(holder2).addNewParent(holder3).addChild(t1).popParent
    builder2.depth shouldBe 1
    val builder3 = builder2.addNewParent(holder4).addChild(t2).addChild(t3)
    builder3.holder shouldBe holder("useCase1", holder("useCase2", holder("useCase4", t3, t2), holder("useCase3", t1)))
    builder3.depth shouldBe 2
    builder3.getCurrentChild shouldBe Some(t3)
  }

}
