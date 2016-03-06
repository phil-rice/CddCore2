package org.cddcore.utilities


class MonitorSpec extends CddSpec {

  "A monitor with Depth" should "prefix logged messages with strings that are a function of the depth" in {
    val monitor = Monitor.remember
    monitor.enter("one")
    monitor("onea")
    monitor.exit("one")
    monitor.enter("two")
    monitor.exit("two")

    monitor.messages shouldBe Vector(
      "one",
      " onea",
      "one",
      "two",
      "two"
    )
  }

  it should "throw an exception if there are more exits than enters" in {
    val monitor = Monitor.remember
    monitor.enter("one")
    monitor("onea")
    monitor.exit("one")
   intercept[MonitorMismatchException] (monitor.exit("one"))
  }

  it should "use the exit function if it exists" in {
    fail
  }

  it should "report an exception in the exit method then throw that exception"

}
