package dsl.la

import scala.reflect.ClassTag
import scala.collection.mutable.WrappedArray

trait Vector[T] {
  protected[la] def underlying: IndexedSeq[T]
  def *(v: Vector[T]): Vector[T]
  def +(v: Vector[T]): Vector[T]
  def map[U: Numeric: ClassTag](v: T => U): Vector[U]
}

object DenseVector {
  def apply[T: Numeric: ClassTag](a: T*): Vector[T] = apply(a.toArray)
  def apply[T: Numeric: ClassTag](v: Array[T]): Vector[T] = new DenseVector(v)
}

final private class DenseVector[T: Numeric: ClassTag](val x: Array[T]) extends Vector[T] {
  def underlying = x
  
  def num = implicitly[Numeric[T]]
  
  def +(v: Vector[T]) = new DenseVector[T](
      ((underlying zip v.underlying) map ((x: (T, T)) =>  num.plus(x._1, x._2))).toArray
  )
  
  def *(v: Vector[T]) = new DenseVector[T](
      ((underlying zip v.underlying) map ((x: (T, T)) =>  num.times(x._1, x._2))).toArray
  )
  
  def map[U: Numeric: ClassTag](f: T => U): Vector[U] = new DenseVector(underlying.map(f).toArray)
  
  override def equals(that: Any) = that match {
    case t: Vector[T] => t.underlying.toSeq == underlying.toSeq
    case _ => false
  }
  
  override def toString = underlying.mkString("DenseVector(", ",", ")")
}


object SparseVector {
  // TODO we need index, value tuples. For that we need tuples
  def apply[T: Numeric: ClassTag](a: T*): Vector[T] = apply(a.toList)
  def apply[T: Numeric: ClassTag](v: List[T]): Vector[T] = new SparseVector(v)
}

final private class SparseVector[T: Numeric: ClassTag](val x: List[T]) extends Vector[T] {
  def underlying: WrappedArray[T] = WrappedArray.make(x.toArray) // imagine here we have abstraction over the sparse representation
  def num = implicitly[Numeric[T]]
  
  // these can be slow, we do not care
  def +(v: Vector[T]) = new DenseVector[T](
      ((underlying zip v.underlying) map ((x: (T, T)) =>  num.plus(x._1, x._2))).toArray
  )
  
  // these can be slow, we do not care
  def *(v: Vector[T]) = new DenseVector[T](
      ((underlying zip v.underlying) map ((x: (T, T)) =>  num.times(x._1, x._2))).toArray
  )
  
  
  def map[U: Numeric: ClassTag](f: T => U): Vector[U] = new SparseVector(underlying.map(f).toList)
    
  override def equals(that: Any) = that match {
    case t: Vector[T] => t.underlying.toSeq == underlying.toSeq
    case _ => false
  }
  
  override def toString = underlying.mkString("SparseVector(", ",", ")")
}