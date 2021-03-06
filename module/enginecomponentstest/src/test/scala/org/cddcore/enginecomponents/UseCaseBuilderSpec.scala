/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package org.cddcore.enginecomponents

import org.cddcore.utilities.{CddSpec, HierarchyBuilder, NullLifeCycle}

import scala.collection.immutable.ListMap


class UseCaseBuilderSpec extends CddSpec {
  def uc(s: String, ec: EngineComponent[Int, String]*) = UseCase[Int, String](s, ec.toList, None, DefinedInSourceCodeAt.definedInSourceCodeAt(1), ListMap(), List())

  val useCase1 = uc("useCase1")
  val useCase2 = uc("useCase2")
  val useCase3 = uc("useCase3")
  val useCase4 = uc("useCase4")

  import Scenario._

  implicit def nullLifeCycle[C] = new NullLifeCycle[C]

  val s1 = 1 produces "result"
  val s2 = 2 produces "result"
  val s3 = 3 produces "result"

  type UC = UseCase[Int, String]
  type Child = EngineComponent[Int, String]

  "A UseCaseBuilder with no operations" should "have the passed in use case and depth 0" in {
    val holder1 = new HierarchyBuilder[UC, Child](useCase1)
    holder1.holder shouldBe useCase1
    holder1.depth shouldBe 0
  }

  "A UseCaseBuilder addChild method with depth 0" should "add children to the use case and not mess with depth" in {
    val holder1 = new HierarchyBuilder[UC, Child](useCase1)
    val holder2 = holder1.addChild(s1).addChild(s2).addChild(s3)
    holder2.holder shouldBe useCase1.copy(components = List(s3, s2, s1))
    holder2.depth shouldBe 0
  }
  "A UseCaseBuilder addNewParent method " should "nest children with new usecases increasing depth" in {
    val holder1 = new HierarchyBuilder[UC, Child](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3)
    holder2.holder shouldBe uc("useCase1", uc("useCase2", uc("useCase3")))
    holder2.depth shouldBe 2
  }
  it should "allow scenarios to be added to current use case" in {
    val holder1 = new HierarchyBuilder[UC, Child](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3).addChild(s1).addChild(s2).addChild(s3)
    holder2.holder shouldBe uc("useCase1", uc("useCase2", uc("useCase3", s3, s2, s1)))
    holder2.depth shouldBe 2
  }
  it should "allow scenarios to be added to current use case, then a pop and another use case added" in {
    val holder1 = new HierarchyBuilder[UC, Child](useCase1)
    val holder2 = holder1.addNewParent(useCase2).addNewParent(useCase3).addChild(s1).popParent
    holder2.depth shouldBe 1
    val holder3 = holder2.addNewParent(useCase4).addChild(s2).addChild(s3)
    holder3.holder shouldBe uc("useCase1", uc("useCase2", uc("useCase4", s3, s2), uc("useCase3", s1)))
    holder3.depth shouldBe 2
  }

}
