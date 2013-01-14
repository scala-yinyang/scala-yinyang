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

  "A deep embedding of la without Rep types" should "compile" in {
    val x = new VectorDSL {
      def main = {
        val v1 = DenseVector(liftTerm(1))
        val res = (v1 + (DenseVector(liftTerm(3), liftTerm(4), liftTerm(5)) * SparseVector[Int](liftTerm(6), liftTerm(7), liftTerm(8))))
        val mappedRes = res.map(_ + liftTerm(1))
        mappedRes
      }
    }
  }

  "Baby steps first: No code at all. Just return a constant!!" should "compile" in {
    val out = 1
    var varOut = 2
    new VectorDSL {
      def main = { val x = DenseVector(out,varOut,3)(null, null); var y = x; ();}
    }
  }
}
