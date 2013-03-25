package collections

import scala.collection.mutable

object List {
  def apply[T](xs: T*): List[T] = new List(xs.toSeq: _*)
  private def apply[T](l: scala.List[T]): List[T] = new List(l.toSeq: _*)
}

class List[T](xs: T*) extends Iterable[T] {
  private[collections] val inner: scala.List[T] = scala.List(xs.toSeq: _*)

  def map[B](f: T ⇒ B): List[B] = List(inner.map(f))

  def flatMap[B](f: T ⇒ List[B]): List[B] = List(inner.flatMap(x ⇒ f(x).inner))

  def filter(f: T ⇒ Boolean): List[T] = List(inner.filter(f))

  def sortBy[B: Ordering](f: T ⇒ B): List[T] = List(inner.sortBy(f))

  def ::(e: T): List[T] = List(e :: inner)

  def ++(l2: List[T]) = List(inner ++ l2.inner)

  def mkString: String = inner.mkString

  def mkString(s: String): String = inner.mkString(s)

  def head: T = inner.head

  def tail: List[T] = List(inner.tail)

  def isEmpty: Boolean = inner.isEmpty

  def drop(n: Int): List[T] = List(inner.drop(n))

  def groupBy[B](f: T ⇒ B): HashMap[B, List[T]] = HashMap(collection.mutable.Map(inner.groupBy(f).toSeq.map(x ⇒ (x._1, List(x._2))): _*))

  def foldLeft[B](z: B)(op: (B, T) ⇒ B): B = inner.foldLeft(z)(op)

  def take(n: Int): List[T] = List(inner.take(n))

  def length: Int = inner.size

  override def toString: String = inner.toString

  override def equals(that: Any) = that match {
    case n: List[_] ⇒ inner.equals(n.inner)
    case _          ⇒ false
  }

}