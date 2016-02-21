package org.cddcore.utilities

import org.scalatest.{FlatSpec, Matchers}

trait CddSpec extends FlatSpec with Matchers {

  def safeMake[X](x: => X) = try {
    x
  } catch {
    case e: Exception => e.printStackTrace(); throw e
  }
}