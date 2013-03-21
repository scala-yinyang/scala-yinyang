import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import lifted._

@RunWith(classOf[JUnitRunner])
class OptiMLSpec extends FlatSpec with ShouldMatchers {

  // "A basic OptiML test should work without YinYang" should "rewire" in {
  //   new OptiML {
  //     def mainDelite(): Any = {
  //     }
  //   }
  // }

  "A basic OptiML test should" should "rewire" in {
    val y = 1
    val x: Int = optiML {
      val x = y
      val b = true
      val c = b && true

      if (b) x + y + x + y + x else x + y + x + y + x

      val z = 1 + x
      val k = z + 1
      k
    }
  }

}

class OptiGraphSpec extends FlatSpec with ShouldMatchers {

  "A basic OptiGraph test should" should "rewire" in {
    val y = 1
    val x: Int = optiGraph {
      val x = y
      val z = 1 + x
      val k = z + 1
      k
    }
  }

}