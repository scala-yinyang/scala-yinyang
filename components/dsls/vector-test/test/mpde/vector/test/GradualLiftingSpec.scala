package mpde.vector.test

import org.scalatest._
import dsl.la._
import dsl.la.norep._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GradualLiftingSpec extends FlatSpec with ShouldMatchers {

  //def pow[T](v: DenseVector[T]): DenseVector[T] = v
  //def pow[T](c: DenseVector[T]): DenseVector[T] = v(1) = "xxx"

  /*
   * How does it work in LMS?
   *   - unstaged
   * TODO: Look into this
   *
   * Interleave DSL code with general Scala code.
   * Pros:
   *   Use libraries that already exist and still benefit from optimization
   *     -- compile-time type checking
   *     -- the effects can be conservative OR can go deeper depending on the dsl: you can use immutable structures
   *   Support for println debugging
   *   The more you stage the more you gain.
   *
   * Cons: Many optimizations can fall out.
   *
   * Possible Extension: purifying the DSL either through a warning system or errors.
   *
   * new DSL extends VectorDSL {
   *
   *   def reflect[T: TypeTag](t: T): Rep[T] // assume effects
   *   def reify[T: TypeTag](t: Rep[T]): T   // assume effects
   *
   *   def main {
   *     val x: Rep[DenseVector[Int]] = DenseVector.apply(Const(20))
   *     val pow: Rep[DenseVector[Int]] =
   *       reflect(pow(reify(x))) // <== reify: decide if the datastructure is mutable or not and add the correct deps
   *     reflect(println(reify(pow)))
   *
   *     val y: Rep[DenseVector[Int]] = DenseVector.apply(Const(20), ...)
   *
   *     val arg_1 = pow + y
   *     reflect(println(reify(arg_1)))
   *
   *     val arg_2 = x + y
   *     reflect(println(reify(arg_2)))
   *   }
   * }
   *
   */

  "Partial lifting" should "produce values" in {
    /*
   val x = DenseVector(20, 31, 44)

   val pow = pow(x) // non-lifted
   println(pow)  // non-lifted

   val y = DenseVector(20, 31, 44)
   println(pow + y) // should be executed before println
     println(x + y) // should be constant folded
     */
  }

}
