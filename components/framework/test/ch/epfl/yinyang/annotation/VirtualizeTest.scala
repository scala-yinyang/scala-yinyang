package ch.epfl.yinyang
package annotation

import org.scalatest.{ FlatSpec, ShouldMatchers }

class VirtualizeSpec extends FlatSpec with ShouldMatchers {

  def __ifThenElse[T](cs: List[Boolean], tb: => T, eb: => T): T = {
    if (cs forall (_ == true)) tb else eb
  }

  "virtualizeTest" should "be virtualized" in {

    @virtualize
    def virtualizeTest(cs: List[Boolean]) = {
      if (cs) "yep" else "nope"
    }

    virtualizeTest(List(true, false)) should be("nope")
    virtualizeTest(List(true, true)) should be("yep")
  }

  "VirtualizeTest" should "be virtualized" in {

    @virtualize
    object VirtualizeTest {
      def apply(cs: List[Boolean]) = if (cs) "yep" else "nope"
    }

    VirtualizeTest(List(true, false)) should be("nope")
    VirtualizeTest(List(true, true)) should be("yep")
  }

  // Need to provide an equivalent of Scala Virtualized's
  // EmbeddedControls for this to work.
  /*
  "defaultTest" should "be virtualized" in {

    @virtualize
    def defaultTest(c: Boolean) = {
      if (c) "yep" else "nope"
    }

    defaultTest(false) should be ("nope")
    defaultTest(true)  should be ("yep")
  }
   */

  "parameter of virtualizeParamTest" should "not be virtualized" in {

    val c = false
    def virtualizeParamTest(
      @virtualize s: String = if (c) "yep" else "nope") = s

    virtualizeParamTest() should be("nope")
  }

  "type parameter of virtualizeTParamTest" should "not be virtualized" in {

    def virtualizeTParamTest[@virtualize T](s: T) = s

    virtualizeTParamTest("nope") should be("nope")
  }
}
