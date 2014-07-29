package ch.epfl.data
package vectorexample
package shallow

class Vector[T: Numeric](val data: Seq[T]) {
  val numeric = implicitly[Numeric[T]]
  def *(d: T): Vector[T] = new Vector(data map (x => numeric.times(x, d)))
  def *(v: Vector[T]): Vector[T] = new Vector(data zip (v.data) map { case (a, b) => numeric.times(a, b) })
  def map[S: Numeric](f: T => S): Vector[S] = new Vector(data map f)
  override def toString: String = s"""Vector(${data mkString ", "})"""
}

object Vector {
  def apply[T: Numeric](dim: Int): Vector[T] = new Vector[T](0 until dim map (x => implicitly[Numeric[T]].zero))
}
