package collections

object Iterable {
  private[collections] def apply[T](v: scala.collection.Iterable[T]): Iterable[T] =
    List(v.toSeq: _*)
}

trait Iterable[T] {
  private[collections] def inner: scala.collection.Iterable[T]

  def foreach(block: T ⇒ Unit): Unit = inner.foreach(block)

  def toList: List[T] = List(inner.toSeq: _*)
}