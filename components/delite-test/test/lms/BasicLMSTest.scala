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
    def unit(i: Int): Int
    val res = lmsDebug { unit(1) * 2 }
    assert(res == 2) // type tag gets screwed somewhere
  }

}
