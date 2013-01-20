package dsl.la.norep

import scala.reflect.ClassTag
import base._

trait Base extends LiftBase

trait IntDSL extends Base {
  self: DoubleDSL ⇒

  type Int = IntOps

  //TODO (TOASK) where do we provide the implementation for this methods (in result DSL object)
  trait IntOps {
    def +(that: Int): Int
    def +(that: Double): Double
    def *(that: Int): Int
    def *(that: Double): Double
    def unary_- : Int
    def toInt: Int
    def toDouble: Double
  }

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = ???
  }

  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: Unit): Unit = ???
  }

  implicit def intOpsToDoubleOps(conv: Int): Double = ???
}

trait DoubleDSL extends Base {
  selfType: IntDSL ⇒

  type Double = DoubleOps

  trait DoubleOps {
    def +(that: Double): Double
    def +(that: Int): Double

    def *(that: Int): Double
    def *(x: Double): Double
    def toInt: Int
    def toDouble: Double
    def unary_- : Double

    //TODO this defs not from original Double and just to provide
    //implementation for Vector operations
    def pow(power: Double)
    def sqrt
  }

  //TODO (TOASK) maybe extends LiftEvidence[scala.Double, DoubleDSL#Double]
  implicit object LiftDouble extends LiftEvidence[scala.Double, Double] {
    def lift(v: scala.Double): Double = ???
  }

  //TODO (TOASK) we can remove it implicit object from here because we have
  //selfType IntDSL
  //  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
  //    def lift(v: Unit): Unit = ???
  //  }
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
trait NumericOps extends IntDSL with DoubleDSL with Base {
  type Numeric[T] = NumericOps[T]

  trait NumericOps[T] {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def times(x: T, y: T): T
    def negate(x: T): T
    def fromInt(x: Int): T
    def toInt(x: T): Int
    def toDouble(x: T): Double

    def zero = fromInt(liftTerm(0))
    def one = fromInt(liftTerm(1))

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

  //TODO find another place
  //TODO (TOASK) problem how to write Double type or where to place this implicit?
  //TODO if it works correctly in IntDSL and check DEPENDENCIES (correctness mixing of DoubleDSL in IntDSL
  //  implicit def intOpsToDoubleOps(conv: Int): Double = ???
}

trait ArrayDSL extends Base with IntDSL with DoubleDSL {
  type Array[T] = ArrayOps[T]

  trait ArrayOps[T] {
    def apply(i: Int): T
    // TODO complete the list of methods
  }

  object Array {
    def apply[T](values: T*): Array[T] = ???
    def fill[T: ClassTag](n: Int)(elem: ⇒ T): Array[T] = ???
    // TODO complete
  }

}
trait BooleanDSL extends Base {
  type Boolean = scala.Boolean

  implicit object LiftBoolean extends LiftEvidence[scala.Boolean, Boolean] {
    def lift(v: scala.Boolean): Boolean = ???
  }
}

trait IfThenElseDSL extends BooleanDSL with Base {

  def __ifThenElse[T](c: ⇒ Boolean, t: T, e: T) = ???
}

//TODO try to remove it and test without TupleDSL
trait TupleDSL extends Base {
  type Tuple2[T1, T2] = Tuple2Ops[T1, T2]

  trait Tuple2Ops[T1, T2] extends AnyRef {
    def _1: T1
    def _2: T2
    def swap: Tuple2[T2, T1]

    // !!!! TODO (TOASK) find place for code lifting (do we need this lifting)
    implicit object LiftTuple2 extends LiftEvidence[scala.Tuple2[T1, T2], Tuple2[T1, T2]] {
      def lift(v: scala.Tuple2[T1, T2]): Tuple2[T1, T2] = ???
    }
  }

  object Tuple2 {
    def apply[T1, T2](x1: T1, x2: T2): Tuple2[T1, T2] = ???

  }
}

trait VectorDSL extends ClassTagOps with IfThenElseDSL with ArrayDSL with TupleDSL with IntDSL with DoubleDSL with NumericOps with Base with Interpret {
  type Vector[T] = VectorOps[T]

  //TODO test and correct its usage
  //  trait VectorTransformer[T] {
  //    def transform(v: Vector[T]): Vector[T]
  //  }

  trait VectorOps[T] {
    //TODO test and correct its usage
    //  implicit object VectorFunction extends VectorTransformer[T] {
    //    def transform(v: Vector[T]): Vector[T] = ???
    //  }
    def *(v: Vector[T]): Vector[T]
    def +(v: Vector[T]): Vector[T]
    def map[U: Numeric: ClassTag](v: T ⇒ U): Vector[U]

    //TODO it's a bad idea to provide here implementation
    def baseVectors: Array[Vector[T]] = ??? //find base vectors

    def partition(fun: T ⇒ Boolean): Tuple2[Vector[T], Vector[T]]

    def dotProduct(v: Vector[T]): T

    def splice(vs: Vector[T]*): Vector[T]

    def spliceT(v: Tuple2[Vector[T], Vector[T]]): Vector[T]

    //    def transform(tr: VectorTransformer[T]): Vector[T]
  }

  object DenseVector {
    def apply[T: Numeric: ClassTag](a: T*): Vector[T] = ???

    //    def apply(a: Double*): Vector[Double] = ???
    //    def apply[T <: Double,  Double: Numeric: ClassTag](a: T*): Vector[T] = ???
    //    def apply[T <: AnyVal: Numeric: ClassTag](a: Map[Int, T]): Vector[T] = ???
  }

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
