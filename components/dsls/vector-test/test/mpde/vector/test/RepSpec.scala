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
      //
      val a = DenseVector(1, 2)
      val b = a
      val c: Vector[Int] = DenseVector(1, 2, 3)
      a.map(x ⇒ x + 2)
      b.map(_ + 2)
      //
      //      //problem to transform
      //      //==> def testing[T >: Nothing <: Any](a: Int, b: T): Nothing = scala.this.Predef.???
      //      //def testing[T](a: Int, b: T) = ???
    }

    ()
  }
}
