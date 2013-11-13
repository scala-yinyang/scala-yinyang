package ch.epfl.lamp.optiml

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage
import optiml.shallow.ops._

@RunWith(classOf[JUnitRunner])
class QPSCDTest extends FlatSpec with ShouldMatchers {

  "QPSCD" should "work" in intercept[scala.NotImplementedError] {
    val res = optiML {
      val HOGWILD = false

      // type BQP = Record {
      //   val Q: DenseMatrix[Double]
      //   val p: DenseVector[Double]
      //   val lbound: DenseVector[Double]
      //   val ubound: DenseVector[Double]
      //   val diag: DenseVector[Double]
      // }

      // type BQP = (DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double])

      def newBQP(_Q: DenseMatrix[Double], _p: DenseVector[Double], _lbound: DenseVector[Double], _ubound: DenseVector[Double], _diag: DenseVector[Double]) =
        (_Q, _p, _lbound, _ubound, _diag)

      def boundQPSCD(q_i: DenseVectorView[Double], i: Int, p_i: Double, lb_i: Double, ub_i: Double, q_ii: Double, x: DenseVector[Double]) = {
        // memoize xk_i since we will write over it with x{k+1}_i
        val xk_i = x(i)
        // compute the ith component of the gradient
        val d_i = x *:* q_i
        val gradf_i = d_i + p_i
        // compute new value for the coordinate, with projection x^(k+1)_i
        val step = max(q_ii, 1e-6)
        val xkp1_i_cand = max(xk_i - gradf_i / step, lb_i)
        val xkp1_i = min(xkp1_i_cand, ub_i)

        // return updated parameter vector (TODO: use untilconverged_deltas)?
        // DenseVector(i -> xpk1_i)
        if (HOGWILD) {
          x(i) = xkp1_i
          x
        } else {
          (0 :: x.length) { p_i => if (i == p_i) xkp1_i else x(p_i) }
        }
      }

      def boundQPSCDEpoch(x: DenseVector[Double], bqp: (DenseMatrix[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseVector[Double]), perm: IndexVector) = {
        if (HOGWILD) {
          // parallel with mutable, racy updates
          for (i <- 0 :: perm.length) {
            val idx = perm(i)
            boundQPSCD(bqp._1(idx), idx, bqp._2(idx), bqp._3(idx), bqp._4(idx), bqp._5(idx), x)
            ()
          }
          x
        } else {
          // sequential: iterate over perm in order, each element in x one at a time
          var i = -1
          untilconverged_buffered[DenseVector[Double]](x, minIter = perm.length - 1, maxIter = perm.length) { x =>
            i += 1
            val idx = perm(i)
            boundQPSCD(bqp._1(idx), idx, bqp._2(idx), bqp._3(idx), bqp._4(idx), bqp._5(idx), x)
          }
        }
      }

      val args = Seq("", "", "")

      val in = args(0)
      val Q = readMatrix("/Q.csv")
      val p = readVector("/p.csv")
      val lb = readVector("/lb.csv")
      val ub = readVector("/ub.csv")

      val x = DenseVector.zeros(p.length)
      val perm = shuffle(0 :: p.length)
      val bqp = newBQP(Q, p, lb, ub, diag(Q))

      // println("finished loading input. Q: " + Q.numRows + " x " + Q.numCols + ", p: " + p.length + ", lb: " + lb.length + ", ub: " + ub.length)
      // if (HOGWILD) println("HogWild!")
      // tic(bqp)

      // toy inputs for testing
      // val x = DenseVector(1.,1.)
      // val perm = (0::2)
      // val bqp = newBQP(DenseMatrix((1.,2.),(3.,4.)), DenseVector(1.,1.), DenseVector(0.,0.), DenseVector(1.,1.), DenseVector(1.,4.))

      val x_star =
        if (HOGWILD) {
          var i = 0
          val xm = x.mutable
          while (i < 1000) {
            boundQPSCDEpoch(xm, bqp, perm)
            i += 1
          }
          xm
        } else {
          untilconverged_buffered[DenseVector[Double]](x, minIter = 999) { x =>
            boundQPSCDEpoch(x, bqp, perm)
          }
        }

      // toc(x_star)
      // println("found x_star: ")
      x_star.pprint

      ()
    }

    // assert(res == ())
  }

}
