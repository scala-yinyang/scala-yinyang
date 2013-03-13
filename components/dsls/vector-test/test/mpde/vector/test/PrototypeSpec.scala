package mpde.vector.test

import org.scalatest._
import collection.mutable.Stack

import dsl.la.norep._
import dsl.la._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PrototypeSpec extends FlatSpec with ShouldMatchers {

  private[this] val pfield = 1
  val field = 1
  var vfield = 1
  def method = 1
  def paramMethod(i: Int) = i

  /*"A shallow embedding of la" should "produce values" in {
    val x = /*laLift*/ {
      val v1 = DenseVector(1, 2, 3)
      (v1 + (DenseVector(3, 4, 5) * SparseVector[Int](6, 7, 8)): Vector[Int]).map(_ + 1)
    }

    x should equal(DenseVector(20, 31, 44))
  }

  /*it should "lift only fields, functions, and variables that are not bound in the DSL scope" in {
    import dsl.la._
    def y(i: Int) = { 1 }
    def function = 1
    val (t1, t2) = (1, 2)

    val dslResult = laLift {
      //      This does not work for now!!
      //      pfield
      //      field
      //      vfield
      //      method
      //      y(1) // we can leave this out for now
      def m = 1
      val in = 1
      in
      m
      function
      t1
    }
  }*/
  val k = 1
  it should "rewire object applications with our own numeric and class tag" in {
    val x = dsl.la.laDebug {
      val y = dsl.la.DenseVector(k, 2, 3)
      import dsl.la
      val z = la.DenseVector(1, 2, 3)
      import dsl.la.TestObject
      val dv = DenseVector(1, 2, 3)
      ()
    }
  }

  it should "rewire language features to methods" in {
    val x = dsl.la.laLift {
      if (true) DenseVector(1, 2, 3) else { DenseVector(1, 2, 3) }
    }
    ()
  }

  it should "closures should be converted into a cake" in {
    val x = dsl.la.laLift {
      dsl.la.DenseVector(1, 2, 3).map(x ⇒ x + 1)
    }
    ()
  }
*/
}

