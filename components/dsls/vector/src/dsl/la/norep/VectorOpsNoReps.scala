package dsl.la.norep

import scala.reflect.ClassTag
import base._
import ch.epfl.lamp.yinyang.api._

trait Base extends BaseYinYang {
  def main(): Any
}

trait IntDSL extends Base {
  self: DoubleDSL with BooleanDSL ⇒

  type Int = IntOps

  //TODO (TOASK) where do we provide the implementation for this methods (in result DSL object)
  // v: When we stage, we should have the provide the implementation that actually lifts the entire thing
  trait IntOps {
    def +(that: Int): Int
    def +(that: Double): Double
    def *(that: Int): Int
    def *(that: Double): Double
    def unary_- : Int
    def toInt: Int
    def toDouble: Double

    def == : Boolean
    def > : Boolean
    def < : Boolean
  }

  //TODO (TOASK) why do we need to provide implementation for this method
  // v: Else Scala won't let you declare the object with abstract methods
  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = null // TODO: Wouldn't this better be a ???
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Int = null
  }

  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: Unit): Unit = ()
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Unit = ()
  }

  implicit object IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int): scala.Int =
      if (x < y) -1 else if (x == y) 0 else 1
  }

  implicit def intOpsToDoubleOps(conv: Int): Double = ???
}

trait DoubleDSL extends Base {
  selfType: IntDSL with BooleanDSL ⇒

  type Double = DoubleOps

  trait DoubleOps {
    def +(that: Double): Double
    def +(that: Int): Double

    def *(that: Int): Double
    def *(x: Double): Double
    def toInt: Int
    def toDouble: Double
    def unary_- : Double

    def == : Boolean
    def > : Boolean
    def < : Boolean

    //implementation for Vector operations
    def pow(power: Double)
    def sqrt
  }

  //TODO (TOASK) maybe extends LiftEvidence[scala.Double, DoubleDSL#Double]
  implicit object LiftDouble extends LiftEvidence[scala.Double, Double] {
    def lift(v: scala.Double): Double = ???
    def hole(tpe: Manifest[Any], symbolId: scala.Int): Double = ???
  }

  implicit object DoubleOrdering extends Ordering[Double] {
    def compare(x: Double, y: Double): scala.Int =
      if (x < y) -1 else if (x == y) 0 else 1
  }
}

trait ClassTagOps extends Base {
  //  ClassTags posed a bit of a challenge: You want to keep the original
  //  class tags created by the compiler, as rewiring everything is too
  //  cumbersome, but at the same time you want to bridge the types from
  //  the original class tag to the new types in the dsl. The way to do
  //  this is to have an implicit conversion from ClassTag[T] to ClassTag[U]
  //  as long as there is a LiftEvidence[T, U] in scope.
  implicit def classTagToOurClassTag[T, U](x: ClassTag[T])(implicit ev: LiftEvidence[T, U]): ClassTag[U] = x.asInstanceOf[ClassTag[U]]
  type ClassTag[T] = scala.reflect.ClassTag[T]
  val ClassTag = scala.reflect.ClassTag
}

//TODO (TOASK) check the correctness of this implementation (correctness for DSL usage)
//maybe implement Ordering to get it complexier
//maybe rename to NumericLifted
trait NumericOps extends IntDSL with DoubleDSL with BooleanDSL with Base {
  type Numeric[T] = NumericOps[T]

  trait NumericOps[T] {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
    def fromInt(x: Int): T
    def toInt(x: T): Int
    def toDouble(x: T): Double

    def zero = fromInt(lift(0))
    def one = fromInt(lift(1))

    def abs(x: T): T = ???
    def signum(x: T): Int = ???

    class Ops(lhs: T) {
      def +(rhs: T) = plus(lhs, rhs)
      def -(rhs: T) = minus(lhs, rhs)
      def *(rhs: T) = times(lhs, rhs)
      def unary_-() = negate(lhs)

      def abs(): T = NumericOps.this.abs(lhs)
      def toInt(): Int = NumericOps.this.toInt(lhs)
      def toDouble(): Double = NumericOps.this.toDouble(lhs)
    }

    implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)
  }

  //companion for trait Numeric Ops
  //here we can provide all implicit objects
  //TODO try to get it working inside object NumericOps
  //  object NumericOps {

  implicit object IntIsIntegral extends Numeric[Int] {
    def plus(x: Int, y: Int): Int = ???
    def minus(x: Int, y: Int): Int = ???
    def times(x: Int, y: Int): Int = ???
    def negate(x: Int): Int = ???

    def fromInt(x: Int): Int = ???
    def toInt(x: Int): Int = ???
    def toDouble(x: Int): Double = ???
  }

  implicit object DoubleIsIntegral extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = ???
    def minus(x: Double, y: Double): Double = ???
    def times(x: Double, y: Double): Double = ???
    def negate(x: Double): Double = ???
    def fromInt(x: Int): Double = ???
    // TODO these need to return the lifted types. This means that Numeric Type needs to be changed to something else.
    def toInt(x: Double): Int = ???
    def toDouble(x: Double): Double = ???
  }
  //  }

}

trait ArrayDSL extends Base with IntDSL with DoubleDSL with BooleanDSL {
  type Array[T] = ArrayOps[T]

  trait ArrayOps[T] {
    def apply(i: Int): T

    def aggregate[B](z: B)(seqop: (B, T) ⇒ B, combop: (B, B) ⇒ B): B

    def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1

    //TODO implement Ordering
    def sort[B](f: (T) ⇒ B)(implicit ord: Ordering[B]): Array[T]

    def sort(implicit ord: Ordering[T]): Array[T]
  }

  object Array {
    def apply[T](values: T*): Array[T] = ???

    //TODO (ASK) - what to do with by name parameters (=> T)
    def fill[T: ClassTag](n: Int)(elem: ⇒ T): Array[T] = ???
  }

}
trait BooleanDSL extends Base {
  type Boolean = scala.Boolean

  implicit object LiftBoolean extends LiftEvidence[scala.Boolean, Boolean] {
    def lift(v: scala.Boolean): Boolean = ???
    def hole(tpe: Manifest[Any], symbolId: Int): Boolean = ???
  }
}

trait IfThenElseDSL extends BooleanDSL with Base {

  def __ifThenElse[T](c: ⇒ Boolean, t: T, e: T) = ???
}

//TODO try to remove it and test without TupleDSL
//trait TupleDSL extends Base {
//  type Tuple2[T1, T2] = Tuple2Ops[T1, T2]
//
//  trait Tuple2Ops[T1, T2] extends AnyRef {
//    def _1: T1
//    def _2: T2
//    def swap: Tuple2[T2, T1]
//
//    // !!!! TODO (TOASK) find place for code lifting (do we need this lifting)
//    implicit object LiftTuple2 extends LiftEvidence[scala.Tuple2[T1, T2], Tuple2[T1, T2]] {
//      def lift(v: scala.Tuple2[T1, T2]): Tuple2[T1, T2] = ???
//    }
//  }
//
//  object Tuple2 {
//    def apply[T1, T2](x1: T1, x2: T2): Tuple2[T1, T2] = ???
//
//  }
//}

trait VectorDSL
  extends ClassTagOps with IfThenElseDSL with ArrayDSL
  with IntDSL with DoubleDSL with NumericOps with Base
  with Interpreted {
  type Vector[T] = VectorOps[T]

  def stagingAnalyze(): List[scala.Int] = Nil

  //TODO (NEW) (TOASK) - where should we provide implementation for methods of VectorOps
  trait VectorOps[T] {

    def *(v: Vector[T]): Vector[T]
    def +(v: Vector[T]): Vector[T]
    def map[U: Numeric: ClassTag](v: T ⇒ U): Vector[U]
    def reconstruct[U: Numeric: ClassTag](v: (T, T) ⇒ U): Vector[U]

    def baseVectors: Array[Vector[T]] //find base vectors

    def partition(fun: T ⇒ Boolean): Tuple2[Vector[T], Vector[T]]

    def dotProduct(v: Vector[T]): T

    def splice(vs: Vector[T]*): Vector[T]

    def spliceT(v: Tuple2[Vector[T], Vector[T]]): Vector[T]

    def transform[U: Numeric: ClassTag](fn: Vector[T] ⇒ Vector[U]): Vector[U]

    //TODO check new methods
    //TODO implement Ordering
    def apply(i: Int): T

    def sort[B](f: (T) ⇒ B)(implicit ord: Ordering[B]): Vector[T]

    def sort(implicit ord: Ordering[T]): Vector[T]

    def corresponds[B](that: Vector[B])(p: (T, B) ⇒ Boolean): Boolean

    def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1
  }

  object DenseVector {
    def apply[T: Numeric: ClassTag](a: T*): Vector[T] = ???

    //    def apply[T <: AnyVal: Numeric: ClassTag](a: Map[Int, T]): Vector[T] = ???
  }

  def interpret[T: Manifest](params: Any*): T = ???

  /**
   * TODO how are we going to translate to objects and yet remain modular and reusable.
   */
  object SparseVector {
    def apply[T: Numeric: ClassTag](a: T*): Vector[T] = ???

    //TODO required to model Map to use
    //    def apply[T: Numeric: ClassTag](a: Map[Int, T]): Vector[T] = ???
  }

  object TestObject {
    def apply[T](a: T*): Vector[T] = ???
  }
}
