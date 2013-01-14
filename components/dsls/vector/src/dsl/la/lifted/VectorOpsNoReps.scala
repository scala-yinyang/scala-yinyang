package dsl.la.norep

import scala.reflect.ClassTag
import base._

trait Base extends LiftBase

trait IntDSL extends Base {
  type Int = IntOps
  
  trait IntOps {
    def +(that: Int): Int
    // TODO complete
  }

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = ???
  }
}

trait NumericOps extends IntDSL with Base {
  type Numeric[T] = NumericOps[T]

  trait NumericOps[T] {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
    def fromInt(x: Int): T
    def toInt(x: T): Int
    def toLong(x: T): Long
    def toFloat(x: T): Float
    def toDouble(x: T): Double
    // TODO complete   
  }
  
  implicit object NumericInt extends Numeric[IntOps] {
    def fromInt(x: IntOps): IntOps = ???
    def minus(x: IntOps, y: IntOps): IntOps = ???
    def negate(x: IntOps): IntOps = ???
    def plus(x: IntOps, y: IntOps): IntOps = ???
    def times(x: IntOps, y: IntOps): IntOps = ???
    // TODO these need to return the lifted types. This means that Numeric Type needs to be changed to something else.
    def toDouble(x: IntOps): Double = ???
    def toFloat(x: IntOps): Float = ???
    def toInt(x: IntOps): IntOps = ???
    def toLong(x: IntOps): scala.Long = ???
    def compare(x: IntOps, y: IntOps): IntOps = ???
  }
}


trait ArrayDSL extends Base {
  type Array[T] = ArrayOps[T]

  trait ArrayOps[T] {
    def apply(i: Int): T
    // TODO complete the list of methods
  }

  object Array {
    def apply[T](values: T*): Array[T] = ???
    // TODO complete
  }

}

trait VectorDSL extends ArrayDSL with IntDSL with NumericOps with Base with Interpret {
  type Vector[T] = VectorOps[T]

  trait VectorOps[T] {
    def *(v: Vector[T]): Vector[T]
    def +(v: Vector[T]): Vector[T]
    def map[U: Numeric: ClassTag](v: T => U): Vector[U]
  }

  object DenseVector {
    def apply[T: Numeric: ClassTag](a: T*): Vector[T] = ???
  }

  /**
   * TODO how are we going to translate to objects and yet remain modular and reusable.
   */
  object SparseVector {
    def apply[T: Numeric: ClassTag](a: T*): Vector[T] = ???
  }

}
