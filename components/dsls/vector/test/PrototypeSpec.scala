import collection.mutable.Stack
import org.scalatest._

import dsl.la._ 
import dsl.la.norep._ 

class PrototypeSpec extends FlatSpec with ShouldMatchers {

  private[this] val pfield = 1
  val field = 1
  var vfield = 1
  def method = 1
  def paramMethod(i: Int) = i
  
  "A shallow embedding of la" should "produce values" in {
     val x = la {
       val v1 = DenseVector(1,2,3)
       (v1 + (DenseVector(3,4,5) * SparseVector[Int](6,7,8)): Vector[Int]).map(_ + 1)
     }
     
     x should equal (DenseVector(20, 31, 44))
  }

  /*it should "compile with just literal" in {
    val x = laLifted("test", 1 + 2)
  }*/
  
  it should "compile with captured values" in {
    def y(i: Int) = {1}
    def function = 1
    val (t1, t2) = (1,2)

    // TODO decide what should be allowed
    // Thoughts: 
    //   outer vars and vals should be accepted  
    //   fields? Why not? This will not create a confusion.
    //   methods? Why not? This could not create a confusion since parameters of methods can be arbitrary Scala. Explicitly mark it or not?
    // Conclusion - for the time being we can lift all identifiers freely. Later we will fine tune the transformer for fields and methods.
    
    val dslResult = laLifted("test", {
//      pfield
//      field
//      vfield
//      method      
//      y(1) // we can leave this out for now
//      function
      def m = 1
      val in = 1 
      in
      m 
      function
      t1
    })
  }
  
  // let's do baby steps first
  it should "work with a simple object" in {
    //val x = laLifted { DenseVector(1, 2) }
    //x shouldBe la.DenseVector(1,2)
  }
  
}
