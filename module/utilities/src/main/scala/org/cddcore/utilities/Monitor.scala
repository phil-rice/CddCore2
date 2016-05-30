/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
