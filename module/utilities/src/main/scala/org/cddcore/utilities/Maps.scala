package org.cddcore.utilities


object Maps {
  implicit def toMapPimper[K, V](map: Map[K, V]) = new MapPimper[K, V](map)
}

object MapOfLists {
  implicit def toMapOfListsPimper[K, V](map: Map[K, List[V]]) = new MapOfListsPimper(map)

  class MapOfListsPimper[K, V](map: Map[K, List[V]]) {
    def addToList(kv: (K, V)): Map[K, List[V]] = kv match {
      case (k, v) => map.get(k).fold(map + (k -> List[V](v)))(list => map + (k -> (list :+ v)))
    }

  }

}

class MapPimper[K, V](m: Map[K, V]) {

  def recursivelyNotFilter(f: PartialFunction[(Any, Any), Boolean]): Map[K, V] = recursivelyNotFilterPrim(m, f).asInstanceOf[Map[K, V]]

  def recursivelyNotFilterPrim(any: Any, f: PartialFunction[(Any, Any), Boolean]): Any =
    any match {
      case m: Map[_, _] =>
        m.foldLeft(Map[Any, Any]()) {
          case (acc, kv@(k, v)) =>
            if (f.isDefinedAt(kv) && f(kv))
              acc
            else
              acc + (recursivelyNotFilterPrim(k, f) -> recursivelyNotFilterPrim(v, f))
        }
      case _ => any
    }

  def recursivelyFilter(f: PartialFunction[(Any, Any), Boolean]): Map[K, V] = recursivelyFilterPrim(m, f).asInstanceOf[Map[K, V]]

  def recursivelyFilterPrim(any: Any, f: PartialFunction[(Any, Any), Boolean]): Any =
    any match {
      case m: Map[_, _] =>
        m.foldLeft(Map[Any, Any]()) {
          case (acc, kv@(k, v)) =>
            if (!f.isDefinedAt(kv) || f(kv) || k.isInstanceOf[Map[_, _]] || v.isInstanceOf[Map[_, _]])
              acc + (recursivelyFilterPrim(k, f) -> recursivelyFilterPrim(v, f))
            else
              acc
        }
      case _ => any
    }

  def recursivelyTransform[K, V](f: PartialFunction[(Any, Any), (Any, Any)]): Map[K, V] = recursivelyTransformPrim(m, f).asInstanceOf[Map[K, V]]

  def recursivelyTransformPrim(any: Any, f: PartialFunction[(Any, Any), (Any, Any)]): Any = any match {
    case m: Map[_, _] =>
      m.foldLeft(Map[Any, Any]()) {
        case (acc, kv@(k, v)) =>
          if (f.isDefinedAt(kv))
            acc + f(kv)
          else
            acc + (recursivelyTransformPrim(k, f) -> recursivelyTransformPrim(v, f))
      }
    case _ => any
  }
}
