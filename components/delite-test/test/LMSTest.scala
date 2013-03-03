import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import lifted._

@RunWith(classOf[JUnitRunner])
class ScalaSpec extends FlatSpec with ShouldMatchers {

  "A shallow embedding of la" should "produce values" in {
    val x = liftDebug {
      val x = 1
      x + 1
    }

    x should equal(2)
  }

}