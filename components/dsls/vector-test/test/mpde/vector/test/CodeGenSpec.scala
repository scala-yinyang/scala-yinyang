package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  "Static code staging" should "work" in {
    val x = liftPrint { //
      val x = 1
      val y = 2
      val z = 4
      println(x + y + z)
      x + y + z
    }
    assert(x == 7, "Should return the value 7!")
  }

  // TODO (Duy) This should work after the holes are made. 
  /*"Dynamic code insertion" should "work" in {
    val x = 1
    val y = 2

    assert(
      liftPrint {//
        val z = 4
        println(x + y + z)
        x + y + z
      } == 7) // should print "7" and return "7"
  }*/
  /*
     |
     V
    val x = 1
    val y = 2
    assert({
      object staged$1 extends dsl.print.PrintDSL {
        def apply(p1: Int, p2: Int) = {
          val z = 4
          println(p1 + p2 + z)
          p1 + p2 + z
        }
      }
      staged$1(x, y)
    } == 7)
  */
}
