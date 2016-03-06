package org.cddcore.utilities


class MonitorSpec extends CddSpec {

  "A monitor with Depth" should "prefix logged messages with strings that are a function of the depth" in {
    val monitor = Monitor.remember
    monitor("one", {
      monitor("onea")
      "someValue"
    }) shouldBe "someValue"

    monitor("two", "someOtherValue") shouldBe "someOtherValue"

    monitor.messages shouldBe Vector(
      "one",
      "  onea",
      "two"
    )
  }



  it should "report an exception in the method then throw that exception" in {
    val monitor = Monitor.remember
    intercept[RuntimeException](monitor("one", {
      monitor("two", throw new RuntimeException("my message"))
      "someValue"
    })).getMessage shouldBe "my message"
    monitor.messages shouldBe Vector(
      "one",
      "  two",
      "  Exception RuntimeException thrown",
      "Exception RuntimeException thrown"
    )
  }

}
