package mpde.vector.test

import dsl.la.rep._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import dsl.la.{ DenseVector, Vector }

/*
 * This tests shows the mechanics of the Rep[T] based approach.
 */
@RunWith(classOf[JUnitRunner])
class RepSpec extends FlatSpec with ShouldMatchers {

  "A deep embedding of la with Rep types" should "compile" in {
    //    val x = new VectorDSL {
    // need this to compile:
    //      def main = {
    //        val v1 = DenseVector(liftTerm(1), liftTerm(2), liftTerm(3))
    //        val res = (v1 + (DenseVector(liftTerm(3), liftTerm(4), liftTerm(5)) * SparseVector[Int](liftTerm(6), liftTerm(7), liftTerm(8))))
    //        val mappedRes = res.map(_ + liftTerm(1))
    //        mappedRes
    //      }
    //    }
  }

  it should "lift Vector to Rep" in {

    val x = dsl.la.laLiftRep {
      //      //val a: Vector[Int] = ???
      //      //val a: Vector[Int] = DenseVector(1, 2)

      //val a: Vector[Int] = ???
      //a: Any

      val a = DenseVector(1, 2)
      //val b = a
      //val c: Vector[Int] = DenseVector(1, 2, 3)
      //a.map(x ⇒ x + 2)
      a.reconstruct((x, y) ⇒ x + y)
      a.reconstruct(_ + _)
      //b.map(_ + 2)
      //
      //      //problem to transform
      //      //==> def testing[T >: Nothing <: Any](a: Int, b: T): Nothing = scala.this.Predef.???
      //      //def testing[T](a: Int, b: T) = ???
    }

    ()
  }

  it should "test ascription lift" in {

    val x = dsl.la.laLiftRep {
      //DenseVector(1, 2)
      DenseVector(1, 2): Vector[Int]
      //DenseVector(1, 2): Any
      //1
      //1: Int
      //1: Double
      //1: Any
      //1: Vector[Int]

      //val a: Vector[Int] = ???
      //a: Any
    }

    ()
  }

  it should "test __ifThenElse" in {

    val x = dsl.la.laLiftRep {
      if (true) 1: Int else 2.0: Double
    }

    ()
  }

  it should "test function type lift" in {

    val x = dsl.la.laLiftRep {
      val a = (x: Int) ⇒ x + 5 //generated$dsllarepVectorDSL12.this.Rep[Int] => generated$dsllarepVectorDSL12.this.Rep[Int]
      val b: Int ⇒ Byte ⇒ Double = ??? //generated$dsllarepVectorDSL12.this.Rep[Int] => generated$dsllarepVectorDSL12.this.Rep[Byte => Double]
      val c: (Int, Byte) ⇒ Double = ??? //(generated$dsllarepVectorDSL12.this.Rep[Int], generated$dsllarepVectorDSL12.this.Rep[Byte]) => generated$dsllarepVectorDSL12.this.Rep[Double]
      val d: Vector[Int] ⇒ Vector[Double] = ??? //generated$dsllarepVectorDSL12.this.Rep[dsl.la.Vector[Int]] => generated$dsllarepVectorDSL12.this.Rep[dsl.la.Vector[Double]]
      val e: List[Int ⇒ Int] = ??? //generated$dsllarepVectorDSL12.this.Rep[List[Int => Int]]
    }

    ()
  }

  it should "test ascription transformation" in {

    val x = dsl.la.laDebugRep {
      val a = 5
      val bvar: Int = 5
      val cvar = DenseVector(1, 2)
      def testMethod(a: Any, c: Vector[Int])(d: Int): Vector[Int] = DenseVector(1, bvar, 3)
      testMethod(bvar, cvar)(2)
    }

    ()
  }
}
