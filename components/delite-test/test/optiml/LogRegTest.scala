package ch.epfl.lamp.optiml

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage
import optiml.shallow.ops._

@RunWith(classOf[JUnitRunner])
class LogRegTest extends FlatSpec with ShouldMatchers {

  "LogReg" should "work" in intercept[java.lang.RuntimeException] {
    val res = optiML {
      val x = readMatrix("")
      val y = readVector("").t

      // println("x.numRows: " + x.numRows)
      // println("x.numCols: " + x.numCols)
      // println("y.length:  " + y.length)

      val theta = DenseVector.zeros(x.numCols)

      // gradient descent with logistic function
      val alpha = 1.0

      val w = untilconverged[DenseVector[Double]](theta, maxIter = 30) { cur =>
        val gradient = sum((0 :: x.numRows) { (i: Int) =>
          x(i) * (y(i) - sigmoid(cur *:* x(i)))
        })

        // println("gradient: ")
        // gradient.pprint

        // alpha*gradient returns an inaccessible type when using implicits (instead of infix)
        val z = cur + gradient * alpha
        // println("next value (c): ")
        // z.pprint
        z
      }
    }

    // assert(res == ())
  }

}
