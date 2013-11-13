// package ch.epfl.lamp.optiml

// import org.scalatest._
// import org.junit.runner.RunWith
// import org.scalatest.junit.JUnitRunner
// import reflect.runtime.universe._
// import ch.epfl.yinyang.runtime.YYStorage
// import optiml.shallow.ops._

// @RunWith(classOf[JUnitRunner])
// class QPSCDTest extends FlatSpec with ShouldMatchers {

//   "QPSCD" should "work" in {
//     val res = optiMLDebug {
//       val HOGWILD = false

//       type BQP = Record {
//         val Q: DenseMatrix[Double]
//         val p: DenseVector[Double]
//         val lbound: DenseVector[Double]
//         val ubound: DenseVector[Double]
//         val diag: DenseVector[Double]
//       }

//       def newBQP(_Q: DenseMatrix[Double], _p: DenseVector[Double], _lbound: DenseVector[Double], _ubound: DenseVector[Double], _diag: DenseVector[Double]): BQP = new Record {
//         val Q = _Q
//         val p = _p
//         val lbound = _lbound
//         val ubound = _ubound
//         val diag = _diag
//       }

//       ()
//     }

//     // assert(res == ())
//   }

// }
