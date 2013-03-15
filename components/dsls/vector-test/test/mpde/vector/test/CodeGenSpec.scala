package mpde.vector.test

import org.scalatest._
import dsl.print._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CodeGenSpec extends FlatSpec with ShouldMatchers {

  "Static code staging" should "work" in {
    val x = liftPrintDebug {
      val x = 1
      val y = 2
      val z = 4
      println(x + y + z)
      x + y + z
    }
    assert(x == 7, "Should return the value 7!")
  }

  "Dynamic code insertion" should "work" in {
    val x = 1
    val y = 2

    assert(
      liftPrint {
        val z = 4
        println(z + x + y) // TODO x + y + z fails miserably
        z + x + y
      } == 7)
  }

  "Compile time code generating" should "work" in {
    val y = 3
    assert(
      liftPrint {
        val b = 0
        println(y)
        break(b) // do not recompile
        1 + b
      } == 1)

  }

  "Runtime code generating" should "not recompile" in {
    val y = 3
    for (i ← 0 to 2) {
      assert(
        liftPrint {
          val b = 0
          println(b)
          break(y)
          1 + b
        } == 1)
    }
  }

  "Runtime code generating" should "recompile" in {
    for (i ← 0 to 2) {
      assert(
        liftPrint {
          val b = 0
          println(b)
          break(i)
          1 + b
        } == 1)
    }
  }

}
