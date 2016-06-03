package org.cddcore.structure

import org.cddcore.utilities.Strings
import sun.awt.EventListenerAggregate

import scala.util.{Success, Try}

case class PathRoot[S](root: S, debug: Boolean)(implicit structure: Structure[S]) {
  def \(path: String): PathRootAndSteps[S] = PathRootAndSteps(root, List(PathStep(true, path)), debug)

  def \\(path: String): PathRootAndSteps[S] = PathRootAndSteps(root, List(PathStep(false, path)), debug)
}

case class PathStep(linked: Boolean, element: String) {
  override def toString() = (if (linked) """\""" else """\\""") + element
}

trait PathResultStrategy[S, A] {
  def resultToAggregate(result: Iterable[S]): Try[A]

}


case class PathResult[S, A, T](convertor: A => T, strategy: PathResultStrategy[S, A]) extends (A => T) {
  def resultToAggregate(result: Iterable[S]) = strategy.resultToAggregate(result)

  def apply(aggregate: A) = convertor(aggregate)
}

case class PathRootAndSteps[S: Structure](root: S, steps: List[PathStep], debug: Boolean) {
  def \(path: String): PathRootAndSteps[S] = PathRootAndSteps(root, steps :+ PathStep(true, path), debug)

  def \\(path: String): PathRootAndSteps[S] = PathRootAndSteps(root, steps :+ PathStep(false, path), debug)

  def \[A, T](result: PathResult[S, A, T]): Path[S, A, T] = Path(this, result, debug)
}

case class Path[S: Structure, A, T](pathRootAndSteps: PathRootAndSteps[S], result: PathResult[S, A, T], debug: Boolean) {
  def apply(): T = get().get

  def get(): Try[T] = {
    val r: Iterable[S] = implicitly[Structure[S]].findResult(pathRootAndSteps)
    result.resultToAggregate(r).map(result)
  }
}

object StringNotFoundInStructureException {
  def apply[S: Structure](result: Iterable[S]): StringNotFoundInStructureException = {
    val resultString = if (result.size > 0) s" which are ${Strings.oneLine(result.map(implicitly[Structure[S]].sToString).mkString(","))}" else ""
    new StringNotFoundInStructureException(result,
      s"Expected one value, got ${result.size}$resultString")
  }
}

case class StringNotFoundInStructureException(result: Iterable[_], msg: String) extends Exception(msg)

trait PathResults[S] {
  implicit def structure: Structure[S]

  object AggregateStrings extends PathResultStrategy[S, String] {
    def resultToAggregate(result: Iterable[S]) = Success(result.foldLeft(StringBuilder.newBuilder)((acc, s) => acc.append(structure.sToString(s))).toString())
  }

  object AggregateOptionString extends PathResultStrategy[S, Option[String]] {
    def resultToAggregate(result: Iterable[S]): Try[Option[String]] =
      if (result.isEmpty) Success(None) else AggregateStrings.resultToAggregate(result).map(Some(_))
  }

  object OneAndOnlyOneString extends PathResultStrategy[S, String] {
    def resultToAggregate(result: Iterable[S]): Try[String] = {
      result.toList match {
        case h :: Nil => AggregateStrings.resultToAggregate(result)
        case l => throw StringNotFoundInStructureException(l)
      }
    }
  }


  def root(s: S, debug: Boolean = false) = PathRoot(s, debug)

  def customPathResult[A, X](fn: A => X, strategy: PathResultStrategy[S, A]) = PathResult(fn, strategy)

  def string = PathResult((s: String) => s, AggregateStrings)

  def optString = PathResult((option: Option[String]) => option, AggregateOptionString)

  def int = PathResult((s: String) => s.toInt, AggregateStrings)

  object Fold {

    class FoldStrategy[Acc, A](mapFn: S => A, initialValue: => Acc, foldFn: (Acc, A) => Acc) extends PathResultStrategy[S, Acc] {
      def resultToAggregate(result: Iterable[S]): Try[Acc] = Try {
        result.map(mapFn).foldLeft(initialValue)(foldFn)
      }
    }

    def fold[Acc, A](mapFn: S => A)(initialValue: => Acc)(foldFn: (Acc, A) => Acc) =
      PathResult((acc: Acc) => acc, new FoldStrategy[Acc, A](mapFn, initialValue, foldFn))

    def string = fold[String, String](structure.sToString) _

    def int = fold[Int, Int](s => structure.sToString(s).toInt) _

    def double = fold[Double, Double](s => structure.sToString(s).toDouble) _

    def list[A](mapFn: S => A) = fold[List[A], A](mapFn)(List())((acc, a) => acc :+ a)

    def reverseList[A](mapFn: S => A) = fold[List[A], A](mapFn)(List())((acc, a) => a :: acc)

    def vector[A](mapFn: S => A) = fold[Vector[A], A](mapFn)(Vector())((acc, a) => acc :+ a)

    def set[A](mapFn: S => A) = fold[Set[A], A](mapFn)(Set())((acc, a) => acc + a)
  }

}