package org.cddcore.utilities

object Lens{
  def identity[X] = Lens[X, X](a => a, (a, b) => b)
}
case class Lens[A, B](get: A => B, set: (A, B) => A) extends Immutable {
  def apply(whole: A): B = get(whole)
  def transform(a: A, f: B => B): A = set(a, f(get(a)))
  def compose[C](that: Lens[C, A]) = Lens[C, B](
    c => get(that.get(c)),
    (c, b) => that.transform(c, set(_, b)))
  def andThen[C](that: Lens[B, C]) = that compose this
}