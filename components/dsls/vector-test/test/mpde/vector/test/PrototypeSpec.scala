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

  "A shallow embedding of la" should "produce values" in {
    val x = /*laLift*/ {
      val v1 = DenseVector(1, 2, 3)
      (v1 + (DenseVector(3, 4, 5) * SparseVector[Int](6, 7, 8)): Vector[Int]).map(_ + 1)
    }

    x should equal(DenseVector(20, 31, 44))
  }

  it should "lift only fields, functions, and variables that are not bound in the DSL scope" in {
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
  }

  // Block(List(
  // ValDef(Modifiers(), newTermName("y"), TypeTree(),
  //   Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))
  // Import(Ident(dsl), List(ImportSelector(newTermName("la"), 1324, newTermName("la"), 1324)))
  // ValDef(Modifiers(), newTermName("z"), TypeTree(),
  //   Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))
  // Import(Select(Ident(dsl), dsl.la), List(ImportSelector(newTermName("DenseVector"), 1381, newTermName("DenseVector"), 1381)))
  // ValDef(Modifiers(), newTermName("dv"), TypeTree(),
  //   Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))
  //), Literal(Constant(()))))
  //
  //

  //  Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))
  //  Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))
  //  Apply(Apply(TypeApply(Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply")), List(TypeTree())), List(Literal(Constant(1)))), List(Select(Select(This(newTypeName("math")), scala.math.Numeric), scala.math.Numeric.IntIsIntegral), Select(Ident(scala.reflect.ClassTag), newTermName("Int")))))

  //  Select(Select(This(newTypeName("$anon")), newTermName("DenseVector")), newTermName("apply"))

  // object out of a cake
  // DenseVector(1,2)
  // Select(Select(Select(Ident(dsl), dsl.la), dsl.la.DenseVector), newTermName("apply"))

  // new VectorDSL { def main = DenseVector(1,2) }
  // Select(Select(This(newTypeName("$anon")), newTermName("DenseVector")), newTermName("apply"))

  // (1,2,3)
  // Select(Select(Ident(scala), scala.Tuple4), newTermName("apply")

  // new Exception()
  // Select(New(Select(Select(Ident(scala), scala.package), newTypeName("Exception"))), nme.CONSTRUCTOR)

  it should "compile method application on an object" in {

    //    val x = laDebug {new VectorDSL { def main = DenseVector.apply(liftTerm(1)) }}
    val x = dsl.la.laLift {
      val y = dsl.la.TestObject(1)
//      import dsl.la
//      val z = la.TestObject(1)
//      import dsl.la.TestObject
//      val dv = TestObject(1)
      ()
    }
  }

  it should "rewire object applications with our own numeric and class tag" in {
    val x = dsl.la.laDebug {
      val y = dsl.la.DenseVector(1,2,3)
      import dsl.la
      val z = la.DenseVector(1,2,3)
      import dsl.la.TestObject
      val dv = DenseVector(1,2,3)
      ()
    }
  }


  it should "compile method application on an object" in {

    //	 val x = laDebug {new VectorDSL { def main = DenseVector.apply(liftTerm(1)) }}
    /*val x = dsl.la.laDebug {
      dsl.la.DenseVector
      ()
    }
    x shouldBe (1, 2, 3, 4)*/
    ()
  }

}
