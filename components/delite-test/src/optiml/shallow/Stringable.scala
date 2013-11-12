package optiml.shallow.ops

import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import ch.epfl.lamp.autolifter.library._
import ch.epfl.lamp.autolifter.annotations._
import ForgeArray._
import ForgeArrayBuffer._
import Numeric._

/**
 * Type class
 */
trait Stringable[T] {
  def makeStr(__arg0: T): String
}

object Stringable extends scala.math.Numeric.ExtraImplicits {

  def strtype[A, B](x: Stringable[A]) = x.asInstanceOf[Stringable[B]]

  /**
   * Type class instances
   */

  // implicit def canStringableTup6[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest,E:Stringable:Manifest,F:Stringable:Manifest]: Stringable[Tup6[A,B,C,D,E,F]] = new Stringable[Tup6[A,B,C,D,E,F]] {
  //   def makeStr(t: Tup6[A,B,C,D,E,F]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+","+t._5.makeStr+","+t._6.makeStr+")"
  //   }
  // }

  implicit def canStringableDouble: Stringable[Double] = new Stringable[Double] {
    def makeStr(__arg0: Double) = {
      "" + __arg0
    }
  }

  // implicit def canStringableTup8[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest,E:Stringable:Manifest,F:Stringable:Manifest,G:Stringable:Manifest,H:Stringable:Manifest]: Stringable[Tup8[A,B,C,D,E,F,G,H]] = new Stringable[Tup8[A,B,C,D,E,F,G,H]] {
  //   def makeStr(t: Tup8[A,B,C,D,E,F,G,H]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+","+t._5.makeStr+","+t._6.makeStr+","+t._7.makeStr+","+t._8.makeStr+")"
  //   }
  // }

  implicit def canStringableDenseMatrix[T: Stringable: Manifest]: Stringable[DenseMatrix[T]] = new Stringable[DenseMatrix[T]] {
    def makeStr(__arg0: DenseMatrix[T]) = {
      __arg0.makeString
    }
  }

  // implicit def canStringableTup5[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest,E:Stringable:Manifest]: Stringable[Tup5[A,B,C,D,E]] = new Stringable[Tup5[A,B,C,D,E]] {
  //   def makeStr(t: Tup5[A,B,C,D,E]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+","+t._5.makeStr+")"
  //   }
  // }

  // implicit def canStringableFloat: Stringable[Float] = new Stringable[Float] {
  //   def makeStr(__arg0: Float) = {
  //     "" + __arg0
  //   }
  // }

  // implicit def canStringableTup2[A:Stringable:Manifest,B:Stringable:Manifest]: Stringable[Tup2[A,B]] = new Stringable[Tup2[A,B]] {
  //   def makeStr(t: Tup2[A,B]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+")"
  //   }
  // }

  // implicit def canStringableTup4[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest]: Stringable[Tup4[A,B,C,D]] = new Stringable[Tup4[A,B,C,D]] {
  //   def makeStr(t: Tup4[A,B,C,D]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+")"
  //   }
  // }

  implicit def canStringableDenseVectorView[T: Stringable: Manifest]: Stringable[DenseVectorView[T]] = new Stringable[DenseVectorView[T]] {
    def makeStr(__arg0: DenseVectorView[T]) = {
      __arg0.makeString
    }
  }

  implicit def canStringableIndexVector: Stringable[IndexVector] = new Stringable[IndexVector] {
    def makeStr(__arg0: IndexVector) = {
      __arg0.makeString
    }
  }

  implicit def canStringableBool: Stringable[Boolean] = new Stringable[Boolean] {
    def makeStr(__arg0: Boolean) = {
      "" + __arg0
    }
  }

  implicit def canStringableStr: Stringable[String] = new Stringable[String] {
    def makeStr(__arg0: String) = {
      __arg0
    }
  }

  // implicit def canStringableTup3[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest]: Stringable[Tup3[A,B,C]] = new Stringable[Tup3[A,B,C]] {
  //   def makeStr(t: Tup3[A,B,C]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+")"
  //   }
  // }

  // implicit def canStringableTup9[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest,E:Stringable:Manifest,F:Stringable:Manifest,G:Stringable:Manifest,H:Stringable:Manifest,I:Stringable:Manifest]: Stringable[Tup9[A,B,C,D,E,F,G,H,I]] = new Stringable[Tup9[A,B,C,D,E,F,G,H,I]] {
  //   def makeStr(t: Tup9[A,B,C,D,E,F,G,H,I]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+","+t._5.makeStr+","+t._6.makeStr+","+t._7.makeStr+","+t._8.makeStr+","+t._9.makeStr+")"
  //   }
  // }

  // implicit def canStringableTup7[A:Stringable:Manifest,B:Stringable:Manifest,C:Stringable:Manifest,D:Stringable:Manifest,E:Stringable:Manifest,F:Stringable:Manifest,G:Stringable:Manifest]: Stringable[Tup7[A,B,C,D,E,F,G]] = new Stringable[Tup7[A,B,C,D,E,F,G]] {
  //   def makeStr(t: Tup7[A,B,C,D,E,F,G]) = {
  //     "("+t._1.makeStr+","+t._2.makeStr+","+t._3.makeStr+","+t._4.makeStr+","+t._5.makeStr+","+t._6.makeStr+","+t._7.makeStr+")"
  //   }
  // }

  implicit def canStringableDenseVector[T: Stringable: Manifest]: Stringable[DenseVector[T]] = new Stringable[DenseVector[T]] {
    def makeStr(__arg0: DenseVector[T]) = {
      __arg0.makeString
    }
  }

  implicit def canStringableVector[T: Stringable: Manifest]: Stringable[Vector[T]] = new Stringable[Vector[T]] {
    def makeStr(__arg0: Vector[T]) = {
      __arg0.makeString
    }
  }

  implicit def canStringableInt: Stringable[Int] = new Stringable[Int] {
    def makeStr(__arg0: Int) = {
      "" + __arg0
    }
  }

  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class Stringable2StringableOps[T: Manifest: Stringable](self: T) {
    def makeStr() = stringable_makestr[T](self)
  }

  def stringable_makestr[T: Manifest: Stringable](__arg0: T): String = implicitly[Stringable[T]].makeStr(__arg0)
}
