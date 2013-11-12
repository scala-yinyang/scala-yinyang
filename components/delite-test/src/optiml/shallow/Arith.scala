package optiml.shallow.ops

// import scala.math.Ordering.Implicits._
// import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._

trait Arith[T] {
  def zero(__arg0: T): T
  def empty: T
  def +(__arg0: T, __arg1: T): T
  def -(__arg0: T, __arg1: T): T
  def *(__arg0: T, __arg1: T): T
  def /(__arg0: T, __arg1: T): T
  def abs(__arg0: T): T
  def exp(__arg0: T): T
  def log(__arg0: T): T
}

object Arith extends scala.math.Numeric.ExtraImplicits {

  /**
   * Type class
   */

  def atype[A, B](x: Arith[A]) = x.asInstanceOf[Arith[B]]

  /**
   * Type class instances
   */

  implicit def canArithDenseVector[T: Arith: Manifest]: Arith[DenseVector[T]] = new Arith[DenseVector[T]] {
    import DenseVector._
    def zero(__arg0: DenseVector[T]) = {
      DenseVector[T](__arg0.length, __arg0.isRow).unsafeImmutable
    }
    def empty = {
      DenseVector[T](0, true).unsafeImmutable
    }
    def +(__arg0: DenseVector[T], __arg1: DenseVector[T]) = {
      __arg0 + __arg1
    }
    def -(__arg0: DenseVector[T], __arg1: DenseVector[T]) = {
      __arg0 - __arg1
    }
    def *(__arg0: DenseVector[T], __arg1: DenseVector[T]) = {
      __arg0 * __arg1
    }
    def /(__arg0: DenseVector[T], __arg1: DenseVector[T]) = {
      __arg0 / __arg1
    }
    def abs(__arg0: DenseVector[T]) = {
      __arg0.abs
    }
    def exp(__arg0: DenseVector[T]) = {
      __arg0.exp
    }
    def log(__arg0: DenseVector[T]) = {
      __arg0.log
    }
  }

  implicit def canArithDenseMatrix[T: Arith: Manifest]: Arith[DenseMatrix[T]] = new Arith[DenseMatrix[T]] {
    import DenseMatrix._
    def zero(__arg0: DenseMatrix[T]) = {
      DenseMatrix[T](__arg0.numRows, __arg0.numCols).unsafeImmutable
    }
    def empty = {
      DenseMatrix[T](0, 0).unsafeImmutable
    }
    def +(__arg0: DenseMatrix[T], __arg1: DenseMatrix[T]) = {
      __arg0 + __arg1
    }
    def -(__arg0: DenseMatrix[T], __arg1: DenseMatrix[T]) = {
      __arg0 - __arg1
    }
    def *(__arg0: DenseMatrix[T], __arg1: DenseMatrix[T]) = {
      __arg0 * __arg1
    }
    def /(__arg0: DenseMatrix[T], __arg1: DenseMatrix[T]) = {
      __arg0 / __arg1
    }
    def abs(__arg0: DenseMatrix[T]) = {
      __arg0.abs
    }
    def exp(__arg0: DenseMatrix[T]) = {
      __arg0.exp
    }
    def log(__arg0: DenseMatrix[T]) = {
      __arg0.log
    }
  }

  implicit def canArithDouble: Arith[Double] = new Arith[Double] {
    def zero(__arg0: Double) = {
      0.0
    }
    def empty = {
      0.0
    }
    def +(__arg0: Double, __arg1: Double) = {
      __arg0 + __arg1
    }
    def -(__arg0: Double, __arg1: Double) = {
      __arg0 - __arg1
    }
    def *(__arg0: Double, __arg1: Double) = {
      __arg0 * __arg1
    }
    def /(__arg0: Double, __arg1: Double) = {
      __arg0 / __arg1
    }
    def abs(__arg0: Double) = {
      __arg0.abs
    }
    def exp(__arg0: Double) = {
      __arg0.exp
    }
    def log(__arg0: Double) = {
      __arg0.log
    }
  }

  // implicit def canArithDenseMatrix[T:Arith:Manifest]: Arith[DenseMatrix[T]] = new Arith[DenseMatrix[T]] {
  //   def zero(__arg0: DenseMatrix[T]) = {
  //     DenseMatrix[T](__arg0.numRows,__arg0.numCols).unsafeImmutable
  //   }
  //   def empty = {
  //     DenseMatrix[T](0,0).unsafeImmutable
  //   }
  //   def +(__arg0: DenseMatrix[T],__arg1: DenseMatrix[T]) = {
  //     densematrix_pl(__arg0,__arg1)
  //   }
  //   def -(__arg0: DenseMatrix[T],__arg1: DenseMatrix[T]) = {
  //     densematrix_sub(__arg0,__arg1)
  //   }
  //   def *(__arg0: DenseMatrix[T],__arg1: DenseMatrix[T]) = {
  //     densematrix_mulclnmul(__arg0,__arg1)
  //   }
  //   def /(__arg0: DenseMatrix[T],__arg1: DenseMatrix[T]) = {
  //     densematrix_div(__arg0,__arg1)
  //   }
  //   def abs(__arg0: DenseMatrix[T]) = {
  //     densematrix_abs(__arg0)
  //   }
  //   def exp(__arg0: DenseMatrix[T]) = {
  //     densematrix_exp(__arg0)
  //   }
  //   def log(__arg0: DenseMatrix[T]) = {
  //     densematrix_log(__arg0)
  //   }
  // }

  // implicit def canArithFloat: Arith[Float] = new Arith[Float] {
  //   def zero(__arg0: Float) = {
  //     0f
  //   }
  //   def empty = {
  //     0f
  //   }
  //   def +(__arg0: Float,__arg1: Float) = {
  //     forge_float_plus(__arg0,__arg1)
  //   }
  //   def -(__arg0: Float,__arg1: Float) = {
  //     forge_float_minus(__arg0,__arg1)
  //   }
  //   def *(__arg0: Float,__arg1: Float) = {
  //     forge_float_times(__arg0,__arg1)
  //   }
  //   def /(__arg0: Float,__arg1: Float) = {
  //     forge_float_divide(__arg0,__arg1)
  //   }
  //   def abs(__arg0: Float) = {
  //     math_object_abs(__arg0).toFloat
  //   }
  //   def exp(__arg0: Float) = {
  //     math_object_exp(__arg0).toFloat
  //   }
  //   def log(__arg0: Float) = {
  //     math_object_log(__arg0).toFloat
  //   }
  // }

  // implicit def canArithInt: Arith[Int] = new Arith[Int] {
  //   def zero(__arg0: Int) = {
  //     0
  //   }
  //   def empty = {
  //     0
  //   }
  //   def +(__arg0: Int,__arg1: Int) = {
  //     forge_int_plus(__arg0,__arg1)
  //   }
  //   def -(__arg0: Int,__arg1: Int) = {
  //     forge_int_minus(__arg0,__arg1)
  //   }
  //   def *(__arg0: Int,__arg1: Int) = {
  //     forge_int_times(__arg0,__arg1)
  //   }
  //   def /(__arg0: Int,__arg1: Int) = {
  //     forge_int_divide(__arg0,__arg1)
  //   }
  //   def abs(__arg0: Int) = {
  //     math_object_abs(__arg0).toInt
  //   }
  //   def exp(__arg0: Int) = {
  //     math_object_exp(__arg0).toInt
  //   }
  //   def log(__arg0: Int) = {
  //     math_object_log(__arg0).toInt
  //   }
  // }

  implicit def canArithInt: Arith[Int] = new Arith[Int] {
    def zero(__arg0: Int) = {
      0
    }
    def empty = {
      0
    }
    def +(__arg0: Int, __arg1: Int) = {
      __arg0 + __arg1
    }
    def -(__arg0: Int, __arg1: Int) = {
      __arg0 - __arg1
    }
    def *(__arg0: Int, __arg1: Int) = {
      __arg0 * __arg1
    }
    def /(__arg0: Int, __arg1: Int) = {
      __arg0 / __arg1
    }
    def abs(__arg0: Int) = {
      __arg0.abs
    }
    def exp(__arg0: Int) = {
      __arg0.exp
    }
    def log(__arg0: Int) = {
      __arg0.log
    }
  }

  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  class Arith2ArithOps[T: Manifest: Arith](self: T) {
    def zero() = arith_zero[T](self)
    def empty() = arith_empty[T]()
    def +(__arg1: T) = arith_pl[T](self, __arg1)
    def -(__arg1: T) = arith_sub[T](self, __arg1)
    def *(__arg1: T) = arith_mul[T](self, __arg1)
    def /(__arg1: T) = arith_div[T](self, __arg1)
    def abs() = arith_abs[T](self)
    def exp() = arith_exp[T](self)
    def log() = arith_log[T](self)
  }

  def arith_zero[T: Manifest: Arith](__arg0: T): T = implicitly[Arith[T]].zero(__arg0)
  def arith_empty[T: Manifest: Arith](): T = implicitly[Arith[T]].empty
  def arith_pl[T: Manifest: Arith](__arg0: T, __arg1: T): T = implicitly[Arith[T]].+(__arg0, __arg1)
  def arith_sub[T: Manifest: Arith](__arg0: T, __arg1: T): T = implicitly[Arith[T]].-(__arg0, __arg1)
  def arith_mul[T: Manifest: Arith](__arg0: T, __arg1: T): T = implicitly[Arith[T]].*(__arg0, __arg1)
  def arith_div[T: Manifest: Arith](__arg0: T, __arg1: T): T = implicitly[Arith[T]]./(__arg0, __arg1)
  def arith_abs[T: Manifest: Arith](__arg0: T): T = implicitly[Arith[T]].abs(__arg0)
  def arith_exp[T: Manifest: Arith](__arg0: T): T = implicitly[Arith[T]].exp(__arg0)
  def arith_log[T: Manifest: Arith](__arg0: T): T = implicitly[Arith[T]].log(__arg0)

  implicit def arith2ArithOps[T: Arith: Manifest](t: T): Arith2ArithOps[T] = new Arith2ArithOps(t)
}
