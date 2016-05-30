package org.cddcore.utilities

object Iterables {
  def guaranteedForeach[X](iterable: TraversableOnce[X])(fn: X => Any) = {
    var exception: Option[Exception] = None
    iterable.foreach(x =>
      try {
        fn(x)
      } catch {
        case (e: Exception) => if (exception.isEmpty) exception = Some(e)
      }
    )
    exception.fold()(throw _)
  }
}
