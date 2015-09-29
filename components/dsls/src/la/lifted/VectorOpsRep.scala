package dsl.la.rep

import ch.epfl.yinyang.api._
import ch.epfl.yinyang.polymorphic._
import ch.epfl.yinyang.polymorphic.generic._
import scala.reflect.ClassTag
import reflect.runtime.universe._
import ch.epfl.yinyang.example._

trait ClassTagOps extends PolymorphicBase {
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

trait NumericOps extends PolymorphicBase {
  type Numeric[T] = NumericOps[T]

  trait NumericOps[T] {
    def plus(x: R[T], y: R[T]): R[T]
    def minus(x: R[T], y: R[T]): R[T]
    def times(x: R[T], y: R[T]): R[T]
    def negate(x: R[T]): R[T]
    def fromInt(x: R[Int]): R[T]
    def toInt(x: R[T]): R[Int]
    def toDouble(x: R[T]): R[Double]

    def zero: R[T]
    def one: R[T]

    def abs(x: R[T]): R[T]
    def signum(x: R[T]): R[Int]

    class Ops(lhs: R[T]) {
      def +(rhs: R[T]) = plus(lhs, rhs)
      def -(rhs: R[T]) = minus(lhs, rhs)
      def *(rhs: R[T]) = times(lhs, rhs)
      def unary_-() = negate(lhs)

      //TODO see compilation problems in implementation
      //this methods are implemented in terms of NumericOps
      def abs(): R[T] = ??? //NumericOps.this.abs(lhs)
      def toInt(): R[Int] = ??? //NumericOps.this.toInt(lhs)
      def toDouble(): R[Double] = ??? //NumericOps.this.toDouble(lhs)
    }

    implicit def mkNumericOps(lhs: R[T]): Ops
  }

  //  val DoubleIsFractional = Numeric.DoubleIsFractional
  //
  //  implicit class NumericOpsOf[T](v: Numeric[T]) {
  //    def plus(x: R[T], y: R[T]): R[T] = ???
  //    def minus(x: R[T], y: R[T]): R[T] = ???
  //    def times(x: R[T], y: R[T]): R[T] = ???
  //    def negate(x: R[T]): R[T] = ???
  //    def fromInt(x: R[Int]): R[T] = ???
  //    def toInt(x: R[T]): R[Int] = ???
  //    def toDouble(x: R[T]): R[Double] = ???
  //
  //    def zero: R[T] = ???
  //    def one: R[T] = ???
  //
  //    def abs(x: R[T]): R[T] = ???
  //    def signum(x: R[T]): R[Int] = ???
  //
  //    implicit def mkNumericOps(lhs: R[T]) = ???
  //  }

  trait NumericOpsOf[T] extends NumericOps[T] {
    def zero: R[T] = ???
    def one: R[T] = ???

    def abs(x: R[T]): R[T] = ???
    def signum(x: R[T]): R[Int] = ???

    override implicit def mkNumericOps(lhs: R[T]) = ???
  }

  object `math.Numeric` {
    implicit object IntIsIntegral extends NumericOpsOf[Int] {
      def plus(x: R[Int], y: R[Int]): R[Int] = ???
      def minus(x: R[Int], y: R[Int]): R[Int] = ???
      def times(x: R[Int], y: R[Int]): R[Int] = ???
      def negate(x: R[Int]): R[Int] = ???

      def fromInt(x: R[Int]): R[Int] = ???
      def toInt(x: R[Int]): R[Int] = ???
      def toDouble(x: R[Int]): R[Double] = ???
    }

    implicit object DoubleIsFractional extends NumericOpsOf[Double] {
      def plus(x: R[Double], y: R[Double]): R[Double] = ???
      def minus(x: R[Double], y: R[Double]): R[Double] = ???
      def times(x: R[Double], y: R[Double]): R[Double] = ???
      def negate(x: R[Double]): R[Double] = ???

      def fromInt(x: R[Int]): R[Double] = ???
      // TODO these need to return the lifted types. This means that Numeric Type needs to be changed to something else.
      def toInt(x: R[Double]): R[Int] = ???
      def toDouble(x: R[Double]): R[Double] = ???
    }
  }
}

trait ScalaVirtualizationDSL extends VirtualControlsBase with VirtualFunctionsBase with VirtualVariablesBase {

  // Members declared in ch.epfl.yinyang.api.Interpreted
  def reset(): Unit = ???

  def $ifThenElse[T](cond: R[Boolean], thenBr: => R[T], elseBr: => R[T]): R[T] = ???
  def $return(expr: R[Any]): R[Nothing] = ???
  def $assign[T](lhs: R[T], rhs: R[T]): R[Unit] = ???
  def $whileDo(cond: R[Boolean], body: => R[Unit]): R[Unit] = ???
  def $doWhile(body: => R[Unit], cond: R[Boolean]): R[Unit] = ???
  def $varDef[T](init: R[T]): R[T] = ???
  def $read[T](init: R[T]): R[T] = ???
  def $lazyValDef[T](init: => R[T]): R[T] = ???
  def $valDef[T](init: R[T]): R[T] = ???
  def $try[T](body: => R[T], b: R[Throwable => T], finalizer: => R[T]): R[T] = ???
  def $throw(e: R[Throwable]): R[Nothing] = ???

  def $app[U](f: R[() => U]): () => R[U] = ???
  def $app[T_1, U](f: R[T_1 => U]): R[T_1] => R[U] = ???
  def $app[T_1, T_2, U](f: R[(T_1, T_2) => U]): (R[T_1], R[T_2]) => R[U] = ???
  def $lam[U](f: () => R[U]): R[() => U] = ???
  def $lam[T_1, U](f: R[T_1] => R[U]): R[T_1 => U] = ???
  def $lam[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): R[(T_1, T_2) => U] = ???

  def infix_hashCode(t: R[Any]): R[Int] = ???
  def infix_!=(lhs: R[Any], rhs: R[Any]): R[Boolean] = ???
}

trait IntDSL extends PolymorphicBase {
  //to overload int operations
  implicit object IntOverloaded

  //Rep versions of Int operations
  trait IntOps {
    def +(that: R[Int]): R[Int]
    def +(that: R[Double])(implicit o: IntOverloaded.type): R[Double]
    def *(that: R[Int]): R[Int]
    def *(that: R[Double])(implicit o: IntOverloaded.type): R[Double]
    def unary_- : R[Int]
    def toInt: R[Int]
    def toDouble: R[Double]
  }

  //implementation of this operations (using implicit conversion to IntOpsOf class
  //before operation
  implicit class IntOpsOf(v: R[Int]) extends IntOps {
    def +(that: R[Int]): R[Int] = ???
    def +(that: R[Double])(implicit o: IntOverloaded.type): R[Double] = ???
    def *(that: R[Int]): R[Int] = ???
    def *(that: R[Double])(implicit o: IntOverloaded.type): R[Double] = ???
    def unary_- : R[Int] = ???
    def toInt: R[Int] = ???
    def toDouble: R[Double] = ???
  }

  implicit object LiftInt extends LiftEvidence[Int, R[Int]] {
    def lift(v: Int): R[Int] = ???
    def hole(tpe: TypeTag[Int], symbolId: Int): R[Int] = ???
  }

  implicit object LiftUnit extends LiftEvidence[scala.Unit, R[Unit]] {
    def lift(v: Unit): R[Unit] = ???
    def hole(tpe: TypeTag[Unit], symbolId: Int): R[Unit] = ???
  }

  //TODO (TOASK) do we need such object
  implicit object IntOrdering extends Ordering[R[Int]] {
    def compare(x: R[Int], y: R[Int]): scala.Int = ???
  }

  //maybe we don't need it
  //  implicit def intOpsToDoubleOps(conv: R[Int]): R[Double] = ???
}

trait DoubleDSL extends PolymorphicBase {

  implicit object DoubleOverloaded

  trait DoubleOps {
    def +(that: R[Int]): R[Double]
    def +(that: R[Double])(implicit o: DoubleOverloaded.type): R[Double]
    def *(that: R[Int]): R[Double]
    def *(that: R[Double])(implicit o: DoubleOverloaded.type): R[Double]
    def unary_- : R[Double]
    def toInt: R[Int]
    def toDouble: R[Double]
  }

  implicit class DoubleOpsOf(v: R[Double]) extends DoubleOps {
    def +(that: R[Int]): R[Double] = ???
    def +(that: R[Double])(implicit o: DoubleOverloaded.type): R[Double] = ???
    def *(that: R[Int]): R[Double] = ???
    def *(that: R[Double])(implicit o: DoubleOverloaded.type): R[Double] = ???
    def unary_- : R[Double] = ???
    def toInt: R[Int] = ???
    def toDouble: R[Double] = ???
  }

  implicit object LiftDouble extends LiftEvidence[Double, R[Double]] {
    def lift(v: Double): R[Double] = ???
    def hole(tpe: TypeTag[Double], symbolId: Int): R[Double] = ???
  }

  implicit object LiftString extends LiftEvidence[String, R[String]] {
    def lift(v: String): R[String] = ???
    def hole(tpe: TypeTag[String], symbolId: Int): R[String] = ???
  }

  //TODO (TOASK) do we need such object
  implicit object DoubleOrdering extends Ordering[R[Double]] {
    def compare(x: R[Double], y: R[Double]): scala.Int = ???
  }
}

trait ArrayDSL extends PolymorphicBase {

  trait ArrayOps[T] {
    def apply(i: R[Int]): R[T]
    // TODO complete the list of methods
  }

  implicit class ArrayOpsOf[T](v: R[Array[T]]) extends ArrayOps[T] {
    def apply(i: R[Int]): R[T] = ???

    def aggregate[B](z: R[B])(seqop: (R[B], R[T]) => R[B], combop: (R[B], R[B]) => R[B]): R[B] = ???

    def fold[A1 >: T](z: R[A1])(op: (R[A1], R[A1]) => R[A1]): R[A1] = ???

    //TODO (NEW) to ask - what type do we need here as output ArrayOps[T] or ArrayOps[R[T]]?
    def sort[B](f: (R[T]) => R[B])(implicit ord: Ordering[R[B]]): R[Array[T]] = ???

    def sort(implicit ord: Ordering[R[T]]): R[Array[T]] = ???
  }

  object Array {
    def apply[T](values: T*): R[Array[T]] = ???

    //TODO (TOASK) (NEW) - what should we do with parameters like elem of type => T
    def fill[T: ClassTag](n: R[Int])(elem: => R[T]): R[Array[T]] = ???
    // TODO complete
  }

}

//trait TupleDSL extends PolymorphicBase {
//
//  trait Tuple2Ops[T1, T2] extends AnyRef {
//    def _1: R[T1]
//    def _2: R[T2]
//    def swap: Tuple2[R[T2], R[T1]]
//  }
//
//  //Wrapper to work with Rep tuples
//  implicit class Tuple2OpsOf[T1, T2](v: R[Tuple2[T1, T2]]) extends Tuple2Ops[T1, T2] {
//    def _1: R[T1] = ???
//    def _2: R[T2] = ???
//    def swap: Tuple2[R[T2], R[T1]] = ???
//  }
//
//  object Tuple2 {
//    def apply[T1, T2](x1: T1, x2: T2): R[Tuple2[T1, T2]] = ???
//  }
//
//}

trait BooleanDSL extends PolymorphicBase {
  implicit object LiftBoolean extends LiftEvidence[Boolean, R[Boolean]] {
    def lift(v: Boolean): R[Boolean] = ???
    def hole(tpe: TypeTag[Boolean], symbolId: Int): R[Boolean] = ???
  }
}

trait MatchingOps extends PolymorphicBase {
  object `ch.epfl.yinyang.runtime.package` {
    object __match {
      def zero: R[Option[Nothing]] = ???
      def one[T](x: R[T]): R[Option[T]] = ???
      def guard[T](cond: R[Boolean], thn: => R[T]): R[Option[T]] = ???
      def runOrElse[T, U](x: T)(f: R[T => Option[U]]): U = ???
    }
  }
}

trait IfThenElseDSL extends PolymorphicBase with BooleanDSL {
  def $ifThenElse[T](c: => R[Boolean], t: R[T], e: R[T]) = ???
}

trait VectorDSL
  extends ArrayDSL with IntDSL with DoubleDSL with ClassTagOps
  with NumericOps with Base with IfThenElseDSL with Interpreted
  with ScalaVirtualizationDSL with MatchingOps with FullyStaged {

  def compile[T: TypeRep, Ret](unstableHoleIds: Set[Int]): Ret = ???
  def generateCode(className: String): String = ???
  def interpret[T: TypeTag](params: Any*) = ???

  type Vector[T] = dsl.la.Vector[T]
  object `dsl.la.Vector` {
    def apply[T: Numeric](v: R[T]*): R[Vector[T]] = ???
  }
  trait VectorOps[T] {
    def *(v: R[Vector[T]]): R[Vector[T]]
    def +(v: R[Vector[T]]): R[Vector[T]]
    def map[U: Numeric: ClassTag](v: R[T] => R[U]): R[Vector[U]]
    def reconstruct[U: Numeric: ClassTag](v: (R[T], R[T]) => R[U]): R[Vector[U]]

    def negate: R[Vector[T]]
    def length: R[Double]

    //returns list of Vectors - to test with Rep Types
    def baseVectors: ArrayOps[R[Vector[T]]] //find base vectors

    def partition(fun: R[T] => R[Boolean]): Tuple2[R[Vector[T]], R[Vector[T]]]

    def dotProduct(v: R[Vector[T]]): R[T]

    def splice(vs: R[Vector[T]]*): R[Vector[T]]

    def spliceT(v: Tuple2[R[Vector[T]], R[Vector[T]]]): R[Vector[T]]

    def transform[U: Numeric: ClassTag](fn: R[Vector[T]] => R[Vector[U]]): R[Vector[U]]

    //TODO check new methods
    //TODO (TOASK) - what ordering should do with Rep?

    def apply(i: R[Int]): R[T]

    def sort[B](f: (R[T]) => R[B])(implicit ord: Ordering[R[B]]): Vector[T]

    def sort(implicit ord: Ordering[R[T]]): Vector[T]

    def corresponds[B](that: Vector[B])(p: (R[T], R[B]) => R[Boolean]): R[Boolean]

    def fold[A1 >: T](z: R[A1])(op: (R[A1], R[A1]) => R[A1]): R[A1]

  }

  implicit class VectorOpsOf[T](v: R[Vector[T]]) extends VectorOps[T] {
    def *(v: R[Vector[T]]): R[Vector[T]] = ???
    def +(v: R[Vector[T]]): R[Vector[T]] = ???
    def map[U: Numeric: ClassTag](v: R[T] => R[U]): R[Vector[U]] = ???
    def reconstruct[U: Numeric: ClassTag](v: (R[T], R[T]) => R[U]): R[Vector[U]] = ???

    def negate: R[Vector[T]] = ???
    def length: R[Double] = ???

    //TODO (TOASK) - is it correct ArrayOps[Rep...] or it should be R[ArrayOps...]
    def baseVectors: ArrayOps[R[Vector[T]]] = ??? //find base vectors

    def partition(fun: R[T] => R[Boolean]): Tuple2[R[Vector[T]], R[Vector[T]]] = ???

    def dotProduct(v: R[Vector[T]]): R[T] = ???

    def splice(vs: R[Vector[T]]*): R[Vector[T]] = ???

    def spliceT(v: Tuple2[R[Vector[T]], R[Vector[T]]]): R[Vector[T]] = ???

    def transform[U: Numeric: ClassTag](fn: R[Vector[T]] => R[Vector[U]]): R[Vector[U]] = ???

    def apply(i: R[Int]): R[T] = ???

    def sort[B](f: (R[T]) => R[B])(implicit ord: Ordering[R[B]]): Vector[T] = ???

    def sort(implicit ord: Ordering[R[T]]): Vector[T] = ???

    def corresponds[B](that: Vector[B])(p: (R[T], R[B]) => R[Boolean]): R[Boolean] = ???

    def fold[A1 >: T](z: R[A1])(op: (R[A1], R[A1]) => R[A1]): R[A1] = ???
  }

  object DenseVector {
    def apply[T: Numeric: ClassTag](a: R[T]*): R[Vector[T]] = ???

    //TODO maybe we need to provide map - test
    def apply[T: Numeric: ClassTag](a: R[Map[Int, T]]): R[Vector[T]] = ???
  }

  /**
   * TODO how are we going to translate to objects and yet remain modular and reusable.
   */
  object SparseVector {
    def apply[T: Numeric: ClassTag](a: R[T]*): R[Vector[T]] = ???

    //TODO (TOASK) - what classes we should model (like Tuples) and what we can use (like Double)
    def apply[T: Numeric: ClassTag](a: R[Map[Int, T]]): R[Vector[T]] = ???
  }

}
