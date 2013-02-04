package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  "Static code staging" should "work" in {
    safe_println("Please ignore the following output (should print 7):")

    assert(liftPrint {
      val x = 1
      val y = 2
      val z = 4
      println(x + y + z)
      returns(x + y + z)
    } == 7) // should print "7" and return "7"
  }

  "Dynamic code insertion" should "work" in {

    safe_println("Please ignore the following output (should print 7, again):")

    val x = 1
    val y = 2

    assert(
      liftPrint {
        val z = 4
        println(x + y + z)
        returns(x + y + z)
      } == 7) // should print "7" and return "7"
  }
}
