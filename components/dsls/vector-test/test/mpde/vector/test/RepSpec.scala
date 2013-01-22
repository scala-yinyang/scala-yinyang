package mpde.vector.test

import dsl.la.rep._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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

    val x = new VectorDSL {
      def produceTuple[T]: Tuple2Ops[Rep[Vector[T]], Rep[Vector[T]]] = ???

      def main = {
        //TODO need way to lift Map to corresponding Rep type
        //        val v0 = DenseVector(Map(1->liftTerm(1.0), 2->liftTerm(2.0), 3->liftTerm(3.0)))

        //        val t0 = DenseVector(liftTerm(1), liftTerm(2.0), liftTerm(3.0))
        val t1: ArrayOps[Rep[Vector[Double]]] = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) baseVectors
        val t2 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0))

        //TODO need way to lift functions to their Rep types
        //        val test2 = t2.partition(_.eq(liftTerm(4)))

        val t3 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) dotProduct t2
        val t4 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) splice (t2, t2)

        //TODO need way to lift Tuple to Rep[Tuple]
        val tuple: Tuple2Ops[Rep[Vector[Double]], Rep[Vector[Double]]] = produceTuple
        val t5 = t4 spliceT (tuple)

        val newV = DenseVector(liftTerm(1), liftTerm(2), liftTerm(3))
        val fn: Rep[Vector[Int]] ⇒ Rep[Vector[Double]] = ???
        val t6: Rep[Vector[Double]] = newV transform (fn)

        val v1 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) negate
        val res = (v1 + (DenseVector(liftTerm(3.0), liftTerm(4.0), liftTerm(5.0)) * SparseVector[Double](liftTerm(6.0), liftTerm(7.0), liftTerm(8.0))))
        val mappedRes = res.map(_ + liftTerm(1.0))
        mappedRes
      }
    }
  }

}
