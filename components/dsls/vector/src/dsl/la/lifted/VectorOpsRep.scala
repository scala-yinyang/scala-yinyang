package dsl.la.rep

import base._
import scala.reflect.ClassTag

/*
 * This is a prototype implementation of the embedded DSL with Rep[T] types. The Rep[T] marks that the value
 * will be available in the next stage of computation. In this prototype we will use the approach similar to LMS.
 * 
 * We need to provide the interface for basic Scala library features. 
 */
trait Base extends LiftBase {
  type Rep[+T]
}

//TODO problem with visibility
//we need to transform Ident to Select(...)
//it's temporary solution
trait ClassTagVals {
  object ClassTag {
    val Int = scala.reflect.ClassTag.Int
    val Double = scala.reflect.ClassTag.Double
  }
}

trait NumericOps extends Base {
  trait NumericOps[T] {
    def plus(x: Rep[T], y: Rep[T]): Rep[T]
    def minus(x: Rep[T], y: Rep[T]): Rep[T]
    def times(x: Rep[T], y: Rep[T]): Rep[T]
    def negate(x: Rep[T]): Rep[T]
    def fromInt(x: Rep[Int]): Rep[T]
    def toInt(x: Rep[T]): Rep[Int]
    def toDouble(x: Rep[T]): Rep[Double]

    def zero: Rep[T]
    def one: Rep[T]

    def abs(x: Rep[T]): Rep[T]
    def signum(x: Rep[T]): Rep[Int]

    class Ops(lhs: Rep[T]) {
      def +(rhs: Rep[T]) = plus(lhs, rhs)
      def -(rhs: Rep[T]) = minus(lhs, rhs)
      def *(rhs: Rep[T]) = times(lhs, rhs)
      def unary_-() = negate(lhs)

      //TODO see compilation problems in implementation
      //this methods are implemented in terms of NumericOps
      def abs(): Rep[T] = ??? //NumericOps.this.abs(lhs)
      def toInt(): Rep[Int] = ??? //NumericOps.this.toInt(lhs)
      def toDouble(): Rep[Double] = ??? //NumericOps.this.toDouble(lhs)
    }

    implicit def mkNumericOps(lhs: Rep[T]): Ops
  }

  trait NumericOpsOf[T] extends NumericOps[T] {
    def plus(x: Rep[T], y: Rep[T]): Rep[T]
    def minus(x: Rep[T], y: Rep[T]): Rep[T]
    def times(x: Rep[T], y: Rep[T]): Rep[T]
    def negate(x: Rep[T]): Rep[T]
    def fromInt(x: Rep[Int]): Rep[T]
    def toInt(x: Rep[T]): Rep[Int]
    def toDouble(x: Rep[T]): Rep[Double]

    def zero: Rep[T] = ???
    def one: Rep[T] = ???

    def abs(x: Rep[T]): Rep[T] = ???
    def signum(x: Rep[T]): Rep[Int] = ???

    override implicit def mkNumericOps(lhs: Rep[T]) = ???
  }

  //concrete implicit objects for Numeric[Double] and Numeric[Int]
  object NumericOpsOf {
    implicit object NumericInt extends NumericOpsOf[Int] {
      def plus(x: Rep[Int], y: Rep[Int]): Rep[Int] = ???
      def minus(x: Rep[Int], y: Rep[Int]): Rep[Int] = ???
      def times(x: Rep[Int], y: Rep[Int]): Rep[Int] = ???
      def negate(x: Rep[Int]): Rep[Int] = ???

      def fromInt(x: Rep[Int]): Rep[Int] = ???
      def toInt(x: Rep[Int]): Rep[Int] = ???
      def toDouble(x: Rep[Int]): Rep[Double] = ???
    }

    implicit object NumericDouble extends NumericOpsOf[Double] {
      def plus(x: Rep[Double], y: Rep[Double]): Rep[Double] = ???
      def minus(x: Rep[Double], y: Rep[Double]): Rep[Double] = ???
      def times(x: Rep[Double], y: Rep[Double]): Rep[Double] = ???
      def negate(x: Rep[Double]): Rep[Double] = ???

      def fromInt(x: Rep[Int]): Rep[Double] = ???
      // TODO these need to return the lifted types. This means that Numeric Type needs to be changed to something else.
      def toInt(x: Rep[Double]): Rep[Int] = ???
      def toDouble(x: Rep[Double]): Rep[Double] = ???
    }
  }
}

trait IntDSL extends Base {
  //to overload int operations
  implicit object IntOverloaded

  //Rep versions of Int operations
  trait IntOps {
    def +(that: Rep[Int]): Rep[Int]
    def +(that: Rep[Double])(implicit o: IntOverloaded.type): Rep[Double]
    def *(that: Rep[Int]): Rep[Int]
    def *(that: Rep[Double])(implicit o: IntOverloaded.type): Rep[Double]
    def unary_- : Rep[Int]
    def toInt: Rep[Int]
    def toDouble: Rep[Double]
  }

  //implementation of this operations (using implicit conversion to IntOpsOf class
  //before operation
  implicit class IntOpsOf(v: Rep[Int]) extends IntOps {
    def +(that: Rep[Int]): Rep[Int] = ???
    def +(that: Rep[Double])(implicit o: IntOverloaded.type): Rep[Double] = ???
    def *(that: Rep[Int]): Rep[Int] = ???
    def *(that: Rep[Double])(implicit o: IntOverloaded.type): Rep[Double] = ???
    def unary_- : Rep[Int] = ???
    def toInt: Rep[Int] = ???
    def toDouble: Rep[Double] = ???
  }

  implicit object LiftInt extends LiftEvidence[Int, Rep[Int]] {
    def lift(v: Int): Rep[Int] = ???
  }

  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: Unit): Unit = ()
  }

  //TODO (TOASK) do we need such object
  implicit object IntOrdering extends Ordering[Rep[Int]] {
    def compare(x: Rep[Int], y: Rep[Int]): scala.Int = ???
  }

  //maybe we don't need it
  //  implicit def intOpsToDoubleOps(conv: Rep[Int]): Rep[Double] = ???
}

trait DoubleDSL extends Base {

  implicit object DoubleOverloaded

  trait DoubleOps {
    def +(that: Rep[Int]): Rep[Double]
    def +(that: Rep[Double])(implicit o: DoubleOverloaded.type): Rep[Double]
    def *(that: Rep[Int]): Rep[Double]
    def *(that: Rep[Double])(implicit o: DoubleOverloaded.type): Rep[Double]
    def unary_- : Rep[Double]
    def toInt: Rep[Int]
    def toDouble: Rep[Double]
  }

  implicit class DoubleOpsOf(v: Rep[Double]) extends DoubleOps {
    def +(that: Rep[Int]): Rep[Double] = ???
    def +(that: Rep[Double])(implicit o: DoubleOverloaded.type): Rep[Double] = ???
    def *(that: Rep[Int]): Rep[Double] = ???
    def *(that: Rep[Double])(implicit o: DoubleOverloaded.type): Rep[Double] = ???
    def unary_- : Rep[Double] = ???
    def toInt: Rep[Int] = ???
    def toDouble: Rep[Double] = ???
  }

  implicit object LiftDouble extends LiftEvidence[Double, Rep[Double]] {
    def lift(v: Double): Rep[Double] = ???
  }

  //TODO (TOASK) do we need such object
  implicit object DoubleOrdering extends Ordering[Rep[Double]] {
    def compare(x: Rep[Double], y: Rep[Double]): scala.Int = ???
  }
}

trait ArrayDSL extends Base {

  trait ArrayOps[T] {
    def apply(i: Rep[Int]): Rep[T]
    // TODO complete the list of methods
  }

  implicit class ArrayOpsOf[T](v: Rep[Array[T]]) extends ArrayOps[T] {
    def apply(i: Rep[Int]): Rep[T] = ???

    def aggregate[B](z: Rep[B])(seqop: (Rep[B], Rep[T]) ⇒ Rep[B], combop: (Rep[B], Rep[B]) ⇒ Rep[B]): Rep[B] = ???

    def fold[A1 >: T](z: Rep[A1])(op: (Rep[A1], Rep[A1]) ⇒ Rep[A1]): Rep[A1] = ???

    //TODO (NEW) to ask - what type do we need here as output ArrayOps[T] or ArrayOps[Rep[T]]?
    def sort[B](f: (Rep[T]) ⇒ Rep[B])(implicit ord: Ordering[Rep[B]]): Rep[Array[T]] = ???

    def sort(implicit ord: Ordering[Rep[T]]): Rep[Array[T]] = ???
  }

  object Array {
    def apply[T](values: T*): Rep[Array[T]] = ???

    //TODO (TOASK) (NEW) - what should we do with parameters like elem of type => T
    def fill[T: ClassTag](n: Rep[Int])(elem: ⇒ Rep[T]): Rep[Array[T]] = ???
    // TODO complete
  }

}

//trait TupleDSL extends Base {
//
//  trait Tuple2Ops[T1, T2] extends AnyRef {
//    def _1: Rep[T1]
//    def _2: Rep[T2]
//    def swap: Tuple2[Rep[T2], Rep[T1]]
//  }
//
//  //Wrapper to work with Rep tuples
//  implicit class Tuple2OpsOf[T1, T2](v: Rep[Tuple2[T1, T2]]) extends Tuple2Ops[T1, T2] {
//    def _1: Rep[T1] = ???
//    def _2: Rep[T2] = ???
//    def swap: Tuple2[Rep[T2], Rep[T1]] = ???
//  }
//
//  object Tuple2 {
//    def apply[T1, T2](x1: T1, x2: T2): Rep[Tuple2[T1, T2]] = ???
//  }
//
//}

trait VectorDSL extends ArrayDSL with IntDSL with DoubleDSL with ClassTagVals with NumericOps with Base with Interpret {

  override def interpret[T]() = {
    val res = main().asInstanceOf[T]
    res
  }

  type Vector[T] = dsl.la.Vector[T]

  trait VectorOps[T] {
    def *(v: Rep[Vector[T]]): Rep[Vector[T]]
    def +(v: Rep[Vector[T]]): Rep[Vector[T]]
    def map[U: Numeric: ClassTag](v: Rep[T] ⇒ Rep[U]): Rep[Vector[U]]

    def negate: Rep[Vector[T]]
    def length: Rep[Double]

    //returns list of Vectors - to test with Rep Types
    def baseVectors: ArrayOps[Rep[Vector[T]]] //find base vectors

    def partition(fun: Rep[T] ⇒ Rep[Boolean]): Tuple2[Rep[Vector[T]], Rep[Vector[T]]]

    def dotProduct(v: Rep[Vector[T]]): Rep[T]

    def splice(vs: Rep[Vector[T]]*): Rep[Vector[T]]

    def spliceT(v: Tuple2[Rep[Vector[T]], Rep[Vector[T]]]): Rep[Vector[T]]

    def transform[U: Numeric: ClassTag](fn: Rep[Vector[T]] ⇒ Rep[Vector[U]]): Rep[Vector[U]]

    //TODO check new methods
    //TODO (TOASK) - what ordering should do with Rep?

    def apply(i: Rep[Int]): Rep[T]

    def sort[B](f: (Rep[T]) ⇒ Rep[B])(implicit ord: Ordering[Rep[B]]): Vector[T]

    def sort(implicit ord: Ordering[Rep[T]]): Vector[T]

    def corresponds[B](that: Vector[B])(p: (Rep[T], Rep[B]) ⇒ Rep[Boolean]): Rep[Boolean]

    def fold[A1 >: T](z: Rep[A1])(op: (Rep[A1], Rep[A1]) ⇒ Rep[A1]): Rep[A1]

  }

  implicit class VectorOpsOf[T](v: Rep[Vector[T]]) extends VectorOps[T] {
    def *(v: Rep[Vector[T]]): Rep[Vector[T]] = ???
    def +(v: Rep[Vector[T]]): Rep[Vector[T]] = ???
    def map[U: Numeric: ClassTag](v: Rep[T] ⇒ Rep[U]): Rep[Vector[U]] = ???

    def negate: Rep[Vector[T]] = ???
    def length: Rep[Double] = ???

    //TODO (TOASK) - is it correct ArrayOps[Rep...] or it should be Rep[ArrayOps...]
    def baseVectors: ArrayOps[Rep[Vector[T]]] = ??? //find base vectors

    def partition(fun: Rep[T] ⇒ Rep[Boolean]): Tuple2[Rep[Vector[T]], Rep[Vector[T]]] = ???

    def dotProduct(v: Rep[Vector[T]]): Rep[T] = ???

    def splice(vs: Rep[Vector[T]]*): Rep[Vector[T]] = ???

    def spliceT(v: Tuple2[Rep[Vector[T]], Rep[Vector[T]]]): Rep[Vector[T]] = ???

    def transform[U: Numeric: ClassTag](fn: Rep[Vector[T]] ⇒ Rep[Vector[U]]): Rep[Vector[U]] = ???

    def apply(i: Rep[Int]): Rep[T] = ???

    def sort[B](f: (Rep[T]) ⇒ Rep[B])(implicit ord: Ordering[Rep[B]]): Vector[T] = ???

    def sort(implicit ord: Ordering[Rep[T]]): Vector[T] = ???

    def corresponds[B](that: Vector[B])(p: (Rep[T], Rep[B]) ⇒ Rep[Boolean]): Rep[Boolean] = ???

    def fold[A1 >: T](z: Rep[A1])(op: (Rep[A1], Rep[A1]) ⇒ Rep[A1]): Rep[A1] = ???
  }

  object DenseVector {
    def apply[T: Numeric: ClassTag](a: Rep[T]*): Rep[Vector[T]] = ???

    //TODO maybe we need to provide map - test
    def apply[T: Numeric: ClassTag](a: Rep[Map[Int, T]]): Rep[Vector[T]] = ???
  }

  /**
   * TODO how are we going to translate to objects and yet remain modular and reusable.
   */
  object SparseVector {
    def apply[T: Numeric: ClassTag](a: Rep[T]*): Rep[Vector[T]] = ???

    //TODO (TOASK) - what classes we should model (like Tuples) and what we can use (like Double)
    def apply[T: Numeric: ClassTag](a: Rep[Map[Int, T]]): Rep[Vector[T]] = ???
  }

}