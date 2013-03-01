package dsl.la

import scala.reflect.ClassTag
import collection.mutable.{ ListBuffer, WrappedArray }
import scala.collection.mutable.WrappedArray

trait Vector[T] {
  //TODO try to implement
  //  type VectorConverter[Vector1[T] <: Vector[T], Vector2[T] <: Vector[T]] = Vector1[T] ⇒ Vector2[T]

  protected[la] def underlying: IndexedSeq[T]
  def *(v: Vector[T]): Vector[T]
  def +(v: Vector[T]): Vector[T]
  def map[U: Numeric: ClassTag](v: T ⇒ U): Vector[U]
  def reconstruct[U: Numeric: ClassTag](v: (T, T) ⇒ U): Vector[U]

  //three simple methods just to example
  //method without parameters which should return List of Vectors (our type)
  def negate: Vector[T]
  //method without parameters which should return Scala standart type
  def length: Double

  def dotProduct(v: Vector[T]): T

  //returns list of Vectors - to test with Rep Types
  def baseVectors: List[Vector[T]] //find base vectors

  //divide vector on 2 vectors according to condition
  //to see behaviour of function with various types as parameter and tuple (result)
  def partition(fun: T ⇒ Boolean): (Vector[T], Vector[T])

  //to see behaviour of varargs parameters with Rep types
  def splice(vs: Vector[T]*): Vector[T]

  //to see behaviour of tuples when they behave as parameter type
  def spliceT(v: (Vector[T], Vector[T])): Vector[T]

  //to see behaviour of functional type parameter with generics
  def transform[U: Numeric: ClassTag](fn: Vector[T] ⇒ Vector[U]): Vector[U]

  def apply(i: Int): T

  //to test implicit parameter
  def sort(implicit ord: Ordering[T]): Vector[T]

  //to test implicit parameter and work with several parameter lists
  def sort[B](f: (T) ⇒ B)(implicit ord: Ordering[B]): Vector[T]

  //nest two functions to test work with functional parameters and bounded type parameter
  def corresponds[B](that: Vector[B])(p: (T, B) ⇒ Boolean): Boolean
  def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1

  //TODO try to implement
  //  def convert[Vector1, Vector2](implicit conv: VectorConverter[Vector1, Vector2]): Vector2
}

object DenseVector {
  def apply[T: Numeric: ClassTag](a: T*): Vector[T] = apply(a.toArray)
  def apply[T: Numeric: ClassTag](v: Array[T]): Vector[T] = new DenseVector(v)

  //TODO change implementation for Dense Vector
  def apply[T: Numeric: ClassTag](v: Map[Int, T]): Vector[T] = new DenseVector(v.values.seq.toArray)

  //Error (see comment in SparseVector)
  //def apply[T: Numeric: ClassTag](v: (T, T, T)): Vector[T] = new DenseVector[T](Array(v._1, v._2, v._3))
}

final private class DenseVector[T: Numeric: ClassTag](val x: Array[T]) extends Vector[T] {
  def underlying = x

  def num = implicitly[Numeric[T]]

  def +(v: Vector[T]) = new DenseVector[T](
    ((underlying zip v.underlying) map ((x: (T, T)) ⇒ num.plus(x._1, x._2))).toArray)

  def *(v: Vector[T]) = new DenseVector[T](
    ((underlying zip v.underlying) map ((x: (T, T)) ⇒ num.times(x._1, x._2))).toArray)

  def map[U: Numeric: ClassTag](f: T ⇒ U): Vector[U] = new DenseVector(underlying.map(f).toArray)

  def reconstruct[U: Numeric: ClassTag](v: (T, T) ⇒ U): Vector[U] = ???

  def negate = new DenseVector[T](underlying.map(num.negate(_)).toArray)

  def length = scala.math.sqrt(num.toDouble(underlying.map((x) ⇒ num.times(x, x)).sum))

  def dotProduct(v: Vector[T]): T = underlying.zip(v.underlying).map((x) ⇒ num.times(x._1, x._2)).sum

  def baseVectors: List[Vector[T]] = underlying.zipWithIndex.map {
    case (x, i) ⇒ new DenseVector({
      val mas = Array.fill(underlying.length)(num.zero)
      mas(i) = x
      mas
    })
  } toList

  def partition(fun: T ⇒ Boolean): (Vector[T], Vector[T]) = underlying partition (fun) match {
    case (head, tail) ⇒ (new DenseVector(head toArray), new DenseVector(tail toArray))
  }

  def splice(vs: Vector[T]*): Vector[T] = new DenseVector(vs flatMap (_.underlying) toArray)

  //warning very ineffective
  def spliceT(v: (Vector[T], Vector[T])): Vector[T] = new DenseVector[T](v._1.underlying ++ v._2.underlying toArray)

  def transform[U: Numeric: ClassTag](fn: Vector[T] ⇒ Vector[U]): Vector[U] = {
    fn(this)
  }

  //TODO provide correct implementation for methods below
  def apply(i: Int): T = underlying(i)

  def sort[B](f: (T) ⇒ B)(implicit ord: Ordering[B]): Vector[T] = this

  def sort(implicit ord: Ordering[T]): Vector[T] = this

  def corresponds[B](that: Vector[B])(p: (T, B) ⇒ Boolean): Boolean = underlying.zipWithIndex.forall(x ⇒ p(x._1, that.apply(x._2)))

  def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1 = z

  //TODO why I can't (shouldn't) setup here type parameter T for DenseVector and SparseVector
  //I mean VectorConverter[DenseVector[T], SparseVector[T]]
  //type VectorConverter[Vector1[T] <: Vector[T], Vector2[T] <: Vector[T]] = Vector1[T]=>Vector2[T]
  //is it because of VectorConverter?
  //  def convert(implicit conv: VectorConverter[DenseVector, SparseVector]): SparseVector[T] = {
  //    conv(this)
  //  }

  override def equals(that: Any) = that match {
    case t: Vector[T] ⇒ t.underlying.toSeq == underlying.toSeq
    case _            ⇒ false
  }

  override def toString = underlying.mkString("DenseVector(", ",", ")")
}

object TestObject {
  def apply[T](v: T*): Vector[T] = ???
}

object SparseVector {
  def apply[T: Numeric: ClassTag](a: T*): Vector[T] = apply(a.toList)
  def apply[T: Numeric: ClassTag](v: List[T]): Vector[T] = new SparseVector(v)

  //ERROR
  //TODO (TOASK) why this apply works if val a = SparseVector[Double](1,2,2) and
  //doesn't work when val a = SparseVector[Int](1,2,2)
  //def apply[T: Numeric: ClassTag](v: (T, T, T)): Vector[T] = new SparseVector[T](List(v._1, v._2, v._3))

  def apply[T: Numeric: ClassTag](v: Map[Int, T]): Vector[T] = new SparseVector(v.values.seq.toList)
}

final private class SparseVector[T: Numeric: ClassTag](val x: List[T]) extends Vector[T] {
  def underlying: WrappedArray[T] = WrappedArray.make(x.toArray) // imagine here we have abstraction over the sparse representation
  def num = implicitly[Numeric[T]]

  // these can be slow, we do not care
  def +(v: Vector[T]) = new DenseVector[T](
    ((underlying zip v.underlying) map ((x: (T, T)) ⇒ num.plus(x._1, x._2))).toArray)

  // these can be slow, we do not care
  def *(v: Vector[T]) = new DenseVector[T](
    ((underlying zip v.underlying) map ((x: (T, T)) ⇒ num.times(x._1, x._2))).toArray)

  def map[U: Numeric: ClassTag](f: T ⇒ U): Vector[U] = new SparseVector(underlying.map(f).toList)

  def reconstruct[U: Numeric: ClassTag](v: (T, T) ⇒ U): Vector[U] = ???

  def negate = new SparseVector[T](underlying.map(num.negate(_)).toList)

  def length = scala.math.sqrt(num.toDouble(underlying.map((x) ⇒ num.times(x, x)).sum))

  def dotProduct(v: Vector[T]): T = underlying.zip(v.underlying).map((x) ⇒ num.times(x._1, x._2)).sum

  def baseVectors: List[Vector[T]] = underlying.zipWithIndex.map {
    case (x, i) ⇒ new SparseVector({
      val listBuffer = ListBuffer.fill(underlying.length)(num.zero)
      listBuffer(i) = x
      listBuffer.toList
    })
  } toList

  def partition(fun: T ⇒ Boolean): (Vector[T], Vector[T]) = underlying partition (fun) match {
    case (head, tail) ⇒ (new SparseVector(head toList), new SparseVector(tail toList))
  }

  def splice(vs: Vector[T]*): Vector[T] = new SparseVector(vs flatMap (_.underlying) toList)

  def spliceT(v: (Vector[T], Vector[T])): Vector[T] = new SparseVector[T](v._1.underlying ++ v._2.underlying toList)

  //TODO change implementation
  def transform[U: Numeric: ClassTag](fn: Vector[T] ⇒ Vector[U]): Vector[U] = {
    fn(this)
  }

  //TODO provide correct implementation for methods below
  def apply(i: Int): T = underlying(i)

  def sort[B](f: (T) ⇒ B)(implicit ord: Ordering[B]): Vector[T] = this

  def sort(implicit ord: Ordering[T]): Vector[T] = this

  def corresponds[B](that: Vector[B])(p: (T, B) ⇒ Boolean): Boolean = underlying.zipWithIndex.forall(x ⇒ p(x._1, that.apply(x._2)))

  def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1 = z

  //  def convert(implicit conv: VectorConverter[SparseVector, DenseVector]): DenseVector[T] = {
  //    conv(this)
  //  }

  override def equals(that: Any) = that match {
    case t: Vector[T] ⇒ t.underlying.toSeq == underlying.toSeq
    case _            ⇒ false
  }

  override def toString = underlying.mkString("SparseVector(", ",", ")")
}
