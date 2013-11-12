package ch.epfl.lamp.optiml

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage
import optiml.shallow.ops._

@RunWith(classOf[JUnitRunner])
class GDATest extends FlatSpec with ShouldMatchers {

  "GDA" should "work" in {
    val res = optiML {
      val x = readMatrix("")
      val y = readVector("").map(d => if (d <= 0.0) false else true)

      /* number of training samples */
      val m = y.length

      /* dimensionality of training data */
      val n = x.numCols

      /* phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the
       * input features are continuous-valued random variables with a multivariate
       * normal distribution.
       *
       * phi is a scalar, mu0 and mu1 are n dimensional vectors,
       * where n is the width of x, and sigma is an n x n matrix.
       */
      val y_zeros = y count { _ == false }
      val y_ones = y count { _ == true }
      val mu0_num = sumRowsIf(0, m) { !y(_) } { x(_) }
      val mu1_num = sumRowsIf(0, m) { y(_) } { x(_) }

      val phi = 1.0 / m * y_ones
      val mu0 = mu0_num / y_zeros
      val mu1 = mu1_num / y_ones

      /* calculate covariance matrix sigma */
      /* x(i) is a row vector for us, while it is defined a column vector in the formula */
      val sigma = sum(0, m) { i =>
        if (y(i) == false) {
          (((x(i) - mu0).t) ** (x(i) - mu0))
        } else {
          (((x(i) - mu1).t) ** (x(i) - mu1))
        }
      }
    }

    // assert(res == ())
  }

}
