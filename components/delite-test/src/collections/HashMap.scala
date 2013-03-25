package collections

import scala.collection.Set
import scala.collection.mutable

object HashMap {
  def apply[K, V]() = new HashMap[K, V](mutable.HashMap[K, V]())
  private[collections] def apply[K, V](hm: mutable.Map[K, V]) = new HashMap[K, V](hm)
}

class HashMap[K, V](hm: mutable.Map[K, V]) extends Iterable[(K, V)] {
  private[collections] val inner: mutable.Map[K, V] = hm

  def apply(k: K): V = inner.apply(k)

  def update(k: K, v: V): Unit = inner.update(k, v)

  def contains(k: K): Boolean = inner.contains(k)

  def size: Int = inner.size

  def values: Iterable[V] = Iterable(inner.values)

  def clear(): Unit = inner.clear()

  def keySet: Set[K] = inner.keySet

  def keys: Iterable[K] = Iterable(inner.keys)

  def getOrElse(key: K, default: ⇒ V): V = inner.getOrElse(key, default)

  def filter(f: (Tuple2[K, V] ⇒ Boolean)): HashMap[K, V] = HashMap(inner.filter(f))

  def flatMap[K2, V2](f: ((K, V)) ⇒ List[(K2, V2)]): HashMap[K2, V2] = HashMap(inner.flatMap { x ⇒ f(x).inner })

  def map[K2, V2](f: ((K, V)) ⇒ (K2, V2)): HashMap[K2, V2] = HashMap((inner.map(x ⇒ f(x))))

  override def toList: List[(K, V)] = List(inner.toSeq: _*)

}