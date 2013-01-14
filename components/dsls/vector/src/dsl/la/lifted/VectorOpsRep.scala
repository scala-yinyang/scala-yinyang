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

trait NumericOps extends Base {
  // TODO complete 
}


trait IntDSL extends Base {
  trait IntOps {
    def +(that: Rep[Int]): Rep[Int] 
    // TODO complete
  }
  
  implicit class IntOpsOf(v: Rep[Int]) extends IntOps {
    def +(that: Rep[Int]): Rep[Int] = ???
  }

  implicit object LiftInt extends LiftEvidence[Int, Rep[Int]] {
    def lift(v: Int): Rep[Int] = ???  
  }
}

trait ArrayDSL extends Base {
  
  trait ArrayOps[T] {
    def apply(i: Rep[Int]): Rep[T] 
    // TODO complete the list of methods
  }
  
  implicit class ArrayOpsOf[T](v: Rep[Array[T]]) extends ArrayOps[T] {
    def apply(i: Rep[Int]): Rep[T] = ??? 
    // TODO complete
  }
  
  object Array {
    def apply[T](values: T*): Rep[Array[T]] = ???
    // TODO complete
  }
  
}

trait VectorDSL extends ArrayDSL with IntDSL with NumericOps with Base {
  
  trait VectorOps[T] {
    def *(v: Rep[Vector[T]]): Rep[Vector[T]]
    def +(v: Rep[Vector[T]]): Rep[Vector[T]]
    def map[U: Numeric: ClassTag](v: Rep[T] => Rep[U]): Rep[Vector[U]]
  }

  implicit class VectorOpsOf[T](v: Rep[Vector[T]]) extends VectorOps[T] {
    def *(v: Rep[Vector[T]]): Rep[Vector[T]] = ???
    def +(v: Rep[Vector[T]]): Rep[Vector[T]] = ???
    def map[U: Numeric: ClassTag](v: Rep[T] => Rep[U]): Rep[Vector[U]] = ???
    // TODO complete
  }
  
  object DenseVector {
    def apply[T: Numeric: ClassTag](a: Rep[T]*): Rep[Vector[T]] = ???
  }

  /**
   * TODO how are we going to translate to objects and yet remain modular and reusable.
   */
  object SparseVector {
    def apply[T: Numeric: ClassTag](a: Rep[T]*): Rep[Vector[T]] = ???
  }

}