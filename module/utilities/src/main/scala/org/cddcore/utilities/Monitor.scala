package org.cddcore.utilities


object Monitor {
  implicit val nullMonitor: Monitor = new Monitor {
    def apply(msg: => String) {}

    def apply[X](enterMsg: => String, x: => X, exitMsg: X => String) = x

  }

  def printlnMonitor: Monitor = new MonitorWithDepth {
    protected def log(msg: String) = println(msg)
  }

  def remember = new RememberMonitor()

}

trait Monitor {

  def apply(msg: => String)

  private def nullMsg[X] = (x: X) => null

  def apply[X](enterMsg: => String, x: => X, exitMsg: X => String = nullMsg): X
}

class MonitorMismatchException extends Exception


trait MonitorWithDepth extends Monitor {
  protected def log(msg: String)

  protected var depth = 0

  protected def indent = " " * (depth * 2)


  def apply(msg: => String) = log(indent + msg)

  def apply[X](enterMsg: => String, x: => X, exitMsg: X => String): X = {
    try {
      enter(enterMsg)
      val result = x
      exit(exitMsg(result))
      result
    } catch {
      case e: Exception =>
        exit(s"Exception ${e.getClass.getSimpleName} thrown")
        throw e
    }
  }

  protected def enter(msg: => String) {
    log(indent + msg)
    depth += 1
  }


  protected def exit(msg: => String): Unit = {
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