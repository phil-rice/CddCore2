package org.cddcore.utilities

class IterablesSpec extends CddSpec {

  "The Iterables guaranteedForeach method" should "call all closures even if one throws an exception, returning the first exception" in {
    val runtimeException2 = new RuntimeException("two")
    val runtimeException4 = new RuntimeException("four")
    val exceptions = List(None, Some(runtimeException2), None, Some(runtimeException4), None)
    var list = List[Int]()
    intercept[RuntimeException](Iterables.guaranteedForeach(List(1, 2, 3, 4, 5).zip(exceptions)) {
      case (i, Some(e)) => list = list :+ i; throw e
      case (i, _) => list = list :+ i
    }) shouldBe runtimeException2
    list shouldBe List(1, 2, 3, 4, 5)
  }
}
