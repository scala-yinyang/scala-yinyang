package mpde.vector.test

import dsl.la.norep._
import dsl.la._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/*
 * This tests shows the mechanics of the Rep[T] based approach.
 */
@RunWith(classOf[JUnitRunner])
class NoRepSpec extends FlatSpec with ShouldMatchers {

  //  "A deep embedding of la without Rep types" should "compile" in {
  //    val x = new VectorDSL {
  //      def main = {
  //
  //        //we need to provide implicits between our new lifted types (NewInt -> NewDouble)
  //        //look implementation of value types!!!
  //        val a = Seq.apply(1, 2, 3.0)
  //        val t0 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0))
  //        val t1 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) baseVectors
  //        val test1: Vector[Double] = t1(liftTerm(0))
  //        val t2 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) partition (_.eq(liftTerm(4)))
  //        val test2: Tuple2[Vector[Double], Vector[Double]] = t2;
  //        val t3 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) dotProduct t2._1
  //        val t4 = DenseVector(liftTerm(1.0), liftTerm(2.0), liftTerm(3.0)) splice (t2._1, t2._2)
  //        val t5: Vector[Double] = t4 spliceT (Tuple2(t2._1, t2._2))
  //
  //        val newV = DenseVector(liftTerm(1), liftTerm(2), liftTerm(3))
  //        val fn: Vector[Int] ⇒ Vector[Double] = ???
  //        val t6: Vector[Double] = newV transform (fn)
  //
  //        //        val v0 = DenseVector(Map(liftTerm(1)->liftTerm(1.0), liftTerm(2)->liftTerm(2.0), liftTerm(3)->liftTerm(3.0))) negate
  //
  //        val v1 = DenseVector(liftTerm(1))
  //        val res = (v1 + (DenseVector(liftTerm(3), liftTerm(4), liftTerm(5)) * SparseVector[Int](liftTerm(6), liftTerm(7), liftTerm(8))))
  //        val mappedRes = res.map(_ + liftTerm(1))
  //        mappedRes
  //      }
  //    }
  //  }
  //
  //  "Baby steps first: No code at all. Just return a constant!!" should "compile" in {
  //    val out = 1
  //    var varOut = 2
  //    new VectorDSL {
  //      def main = { val x = DenseVector(out, varOut, 3)(null, null); var y = x; (); }
  //    }
  //  }

  //  it should "lift-tuple" in {
  //    val x = dsl.la.laDebug {
  //      Tuple4(2, 4, 6, 7)
  //    }
  //    ()
  //  }
  //
  //  it should "lift 1 to Rep" in {
  //    val x = dsl.la.laDebug {
  //      1
  //    }
  //    ()
  //  }

  //  it should "lift vat = 1 to Rep" in {
  //    val x = dsl.la.laDebug {
  //      5 //DenseVector(1)
  //    }
  //    ()
  //  }
  //

  //  it should "lift vat = 1 to Rep" in {
  //
  //    class test extends VectorDSL {
  //      def main() = {
  //        val a: dsl.la.Vector[Int] = DenseVector.apply(test.this.liftTerm(1));
  //        val b: dsl.la.Vector[Int] = a;
  //        test.this.liftTerm(())
  //      }
  //    };
  //    new test().interpret()
  //
  //  }

  //    val r: scala.Int = 5
  it should "lift vat = 1 to Rep" in {

    val x = dsl.la.laDebug {
      val a: Vector[Int] = DenseVector(1, 2)
    }
    ()
  }

}
