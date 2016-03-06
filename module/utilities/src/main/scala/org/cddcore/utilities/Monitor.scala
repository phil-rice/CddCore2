package org.cddcore.utilities


object Monitor {
  implicit val nullMonitor = new Monitor {
    def enter(msg: => String) {}

    def apply(msg: => String) {}

    def exit(msg: => String) {}
  }

  def printlnMonitor = new MonitorWithDepth {
    protected def log(msg: String) = println(msg)
  }

  def remember = new RememberMonitor()

}

trait Monitor {
  def enter(msg: => String)

  def apply(msg: => String)

  def exit(msg: => String)

  private def nullMsg[X] = (x: X) => null

  def apply[X](enterMsg: => String, exitMsg: X => String = nullMsg)(x: => X): X = {
    val m = enterMsg
    try {
      enter(enterMsg)
      val result = x
      exit(exitMsg(x))
      result
    } catch {
      case e: Exception =>
        exit(s"Exception ${e.getClass.getSimpleName} thrown")
        throw e
    }
  }
}

class MonitorMismatchException extends Exception


trait MonitorWithDepth extends Monitor {
  protected def log(msg: String)

  protected var depth = 0

  protected def indent = " " * (depth*2)


  def enter(msg: => String) {
    log(indent + msg)
    depth += 1
  }

  def apply(msg: => String) = log(indent + msg)

  def exit(msg: => String): Unit = {
    depth -= 1
    if (depth < 0) throw new MonitorMismatchException
    if (msg != null) log(indent + msg)
  }
}

class RememberMonitor extends MonitorWithDepth {
  private var list = Vector[String]()

  protected def log(msg: String): Unit = {
    list = list :+ msg
  }

  def messages = list
}