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
  }
}
