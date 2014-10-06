package ch.epfl.yinyang
package annotation

import org.scalatest.{ FlatSpec, ShouldMatchers }

class VirtualizeSpec extends FlatSpec with ShouldMatchers with EmbeddedControls {

  def __ifThenElse[T](cs: List[Boolean], tb: => T, eb: => T): T = {
    if (cs forall (_ == true)) tb else eb
  }

  def infix_==[T](x1: List[T], x2: List[T]): Boolean = {
    (x1 zip x2) forall (p => p._1 == p._2)
  }

  "virtualizeIfTest" should "be virtualized" in {

    @virtualize
    def virtualizeIfTest(cs: List[Boolean]) = if (cs) "yep" else "nope"

    virtualizeIfTest(List(true, false)) should be("nope")
    virtualizeIfTest(List(true, true)) should be("yep")
  }

  "VirtualizeIfTest" should "be virtualized" in {

    @virtualize
    object VirtualizeIfTest {
      def apply(cs: List[Boolean]) = if (cs) "yep" else "nope"
    }

    VirtualizeIfTest(List(true, false)) should be("nope")
    VirtualizeIfTest(List(true, true)) should be("yep")
  }

  "VirtualizeIfTraitTest" should "be virtualized" in {

    @virtualize
    trait VirtualizeIfTrait {
      def apply(cs: List[Boolean]) = if (cs) "yep" else "nope"
    }

    object VirtualizeIfTraitTest extends VirtualizeIfTrait

    VirtualizeIfTraitTest(List(true, false)) should be("nope")
    VirtualizeIfTraitTest(List(true, true)) should be("yep")
  }

  // Should use default `__ifThenElse` from EmbeddedControls.
  "defaultIfTest" should "be virtualized" in {

    @virtualize
    def defaultIfTest(c: Boolean) = if (c) "yep" else {
      var x = "no"
      x + "pe"
    }

    defaultIfTest(false) should be("nope")
    defaultIfTest(true) should be("yep")
  }

  // Should use inner virtualized `__ifThenElse`
  "virtualizeInnerIfTest" should "be virtualized" in {

    // This overrides the `__ifThenElse` in `EmbeddedControls`
    def __ifThenElse[T](c: Boolean, thenBr: => T, elseBr: => T): T =
      if (!c) thenBr else elseBr

    @virtualize
    def virtualizeInnerIfTest(c: Boolean) = if (c) "yep" else "nope"

    virtualizeInnerIfTest(false) should be("yep")
    virtualizeInnerIfTest(true) should be("nope")
  }

  "virtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    def virtualizeEqualsTest(a: List[Boolean], b: List[Boolean]) = a == b

    virtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    virtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  "VirtualizeEqualsTest" should "be virtualized" in {

    @virtualize
    object VirtualizeEqualsTest {
      def apply(a: List[Boolean], b: List[Boolean]) = a == b
    }

    VirtualizeEqualsTest(List(true, true), List(true, false)) should be(false)
    VirtualizeEqualsTest(List(true, true), List(true, true, false)) should be(true)
    (List(true, true) == List(true, true, false)) should be(false)
  }

  // Should use default `Any.==` method from EmbeddedControls.
  "defaultEqualsTest" should "be virtualized" in {

    @virtualize
    def defaultEqualsTest(a: Boolean, b: Boolean) = a == b

    defaultEqualsTest(false, true) should be(false)
    defaultEqualsTest(true, true) should be(true)
  }

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

  "try expression in virtualizeTryTest" should "not be virtualized" in {

    @virtualize
    def virtualizeTryTest[T](s: => T) = try s

    virtualizeTryTest("nope") should be("nope")
  }

  "throw expression in virtualizeThrowTest" should "not be virtualized" in {

    case class MyException(msg: String) extends Exception

    @virtualize
    def virtualizeThrowTest(e: String) = throw MyException(e)

    try {
      virtualizeThrowTest("nope")
    } catch {
      case MyException(e) => e should be("nope")
    }
  }

  "isInstanceOf and asInstanceOf" should "not be virtualized" in {

    @virtualize
    def virtualizeInstanceOf(o: Object) = if (o.isInstanceOf[String]) o.asInstanceOf[String] else null

    virtualizeInstanceOf("hello") should be("hello")
    virtualizeInstanceOf(Nil) should be(null)
  }
}
