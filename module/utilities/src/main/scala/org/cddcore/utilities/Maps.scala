/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
