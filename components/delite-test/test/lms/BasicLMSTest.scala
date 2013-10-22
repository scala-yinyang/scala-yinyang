package lms

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import reflect.runtime.universe._
import ch.epfl.yinyang.runtime.YYStorage

@RunWith(classOf[JUnitRunner])
class BasicLMSTest extends FlatSpec with ShouldMatchers {

  "Simple LMS expression" should "work" in {
    val res = lms {
      val x = 1
      x * 2
    }
    assert(res == 2)
  }

  "Captured identifiers in simple LMS expression" should "work" in {
    // val x = 1
    // val res = lmsDebug {
    //   x * 2
    // }
    // Generates "macros can not be partially evaluated":
    //    class generated$lmsScalaDSL3 extends ScalaDSL {
    //      var captured$x = x;
    //      def main(): Any = (this.lift(captured$x).$times((this.lift(2): Rep[Int])): Rep[Int])
    //    };
    //    {
    //      val dslInstance = ch.epfl.yinyang.runtime.YYStorage.lookup(6418958475914133344L, new generated$lmsScalaDSL3());
    //      val values: Seq[Any] = Seq();
    //      val refs: Seq[Any] = Seq(x);
    //      __assign(dslInstance.captured$x, x);
    //      def recompile(): _root_.scala.Function0[Any] = dslInstance.compile[Int, _root_.scala.Function0[Int]];
    //      val program = ch.epfl.yinyang.runtime.YYStorage.checkRef[_root_.scala.Function0[Int]](6418958475914133344L, values, refs, recompile);
    //      program.apply()
    //    }
    // assert(res == 2) // type tag gets screwed somewhere
  }

  "Virtualization in LMS" should "work" in {
    val res = lms {
      var x = 1
      var i = 0
      x = 2
      if (x == 2) while (i < x) i += 1
      x
    }

    assert(res == 2)
  }

  "Arrays in LMS" should "work" in {
    //val res = lmsDebug {
    //  val arr = new Array[Int](100)
    //  2
    //}

    //class generated$lmsScalaDSL4 extends lms.ScalaDSL {
    //  def main(): Any = {
    //    val arr: Rep[Array[Int]] = ((new Rep[Array[Int]]((this.lift(100): Rep[Int])): Rep[Array[Int]]): Rep[Array[Int]]);
    //    this.lift(2)
    //  }
    //}

    //assert(res == 2)
  }

  "Lists and HOF in LMS" should "work" in {
    //val res = lmsDebug {
    //  val x = List(1)
    //  x.map(_ + 1).head
    //}
    // Urgh: Type arguments are Rep[T]. CanBuild from does not exists as a type or as method on the list object.
    //class generated$lmsScalaDSL3 extends ScalaDSL {
    //  type List[T] = scala.List[T]
    //  def main(): Any = {
    //    val x: Rep[List[Int]] = ((List.apply[Rep[Int]]((this.lift(1): Rep[Int])): Rep[List[Int]]): Rep[List[Int]]);
    //    (x.map[Rep[Int], Rep[List[Int]]]((((x$1: Rep[Int]) => x$1.$plus((this.lift(1): Rep[Int]))): scala.Function1[Rep[Int], Rep[Int]]))((List.canBuildFrom[Rep[Int]]: Rep[CanBuildFrom[List, Int, List[Int]]])): Rep[List[Int]]).head
    //  }
    //}
    //assert(res == 2)
  }

}
