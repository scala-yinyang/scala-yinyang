package lms

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage

@RunWith(classOf[JUnitRunner])
class BasicLMSTest extends FlatSpec with ShouldMatchers {

  "Simple LMS expression" should "work" in {
      val x = 1
      val res = lms{ x }
      assert(res == 2)
  }

}
