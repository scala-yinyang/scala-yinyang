package dsl.print

import ch.epfl.lamp.mpde.api._
import base._

/** The int printing DSL */
trait PrintDSL extends ScalaCompile with CodeGenerator with base.LiftBase with MiniIntDSL with MiniPrintDSL with base.Interpret {

  var sb: StringBuffer = new StringBuffer()

  // hey, we can refine println -- but we don't need it right now
  def println(x: Any) = sb.append(s"scala.Predef.println(${x.toString});\n")

  def generateCode(className: String): String = {
    val res = main()

    s"""
      class $className extends Function0[Any] {
        def apply() = {
          ${sb.toString} + ${res.toString}
        }
      }
    """
  }

  override def interpret[T](): T = {
    if (compiledCode == null) {
      val res = main()
      compiledCode = compile[T]
    }
    compiledCode.apply().asInstanceOf[T]
  }

  var compiledCode: () ⇒ Any = _

  // TODO (Duy) make the mechanism with holes work. This can be interesting for both compile time compilation
  // and for runtime compilation. This could be one of the strong points in our approach.
  /*
   * val y = 1
   * object x { val y = "a" }
   * liftDSL {
   *   print(y + x.y)
   * }
   *
   * -
   * |
   * U
   *
   * liftDSL {
   *   print(hole("p$1") + hole("p$2"))
   * }
   * -
   * |
   * U
   *
   * object staged$1 {
   *   def apply(p$1: Int, p$2: String): Any =
   *     print(p$1 + p$2)
   * }
   * staged$1(y, x.y)
   *
   *
   */

  def hole[T](variable: String): T = ???

}

trait MiniIntDSL extends base.LiftBase {

  type Int = IntOps

  trait IntOps {
    def +(that: Int): Int = IntPlus(IntOps.this, that)
  }

  // actual classes that provide lifting
  case class IntConst(i: scala.Int) extends IntOps { override def toString = s"$i" }
  case class IntPlus(l: IntOps, r: IntOps) extends IntOps { override def toString = s"($l + $r)" }

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = IntConst(v)
  }
}

trait MiniUnitDSL extends base.LiftBase {
  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: Unit): Unit = ()
  }
}

trait MiniPrintDSL extends base.LiftBase {
  def println(x: Any): Unit
}
