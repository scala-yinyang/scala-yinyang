package dsl.print

import ch.epfl.lamp.mpde.api._
import base._

/** The int printing DSL */
trait PrintDSL extends ScalaCompile with CodeGenerator with base.LiftBase with MiniIntDSL with MiniPrintDSL with base.Interpret {

  var sb: StringBuffer = new StringBuffer()

  // hey, we can refine println -- but we don't need it right now
  def println(x: Any) = sb.append(s"scala.Predef.println(${x.toString});\n")

  def break(x: Int) = sb.append("scala.Predef.println(\"--\");\n" * x.value)

  def generateCode(className: String): String = {
    val res = main()
    s"""
      class $className extends Function0[Any] {
        def apply() = {
          ${sb.toString} + ${res.toString}
        }
      }
      // new $className().apply()
    """
  }

  override def interpret[T](): T = {
    if (compiledCode == null) {
      compiledCode = compile[T]
    }
    compiledCode.apply().asInstanceOf[T]
  }

  var compiledCode: () ⇒ Any = _
}

trait MiniIntDSL extends base.LiftBase { self: CodeGenerator ⇒

  type Int = IntOps

  /*
   * Supposed to be in `LiftBase`.
   */
  trait HoleEvidence[Ret] {
    def emit(symbolId: scala.Int): Ret
  }

  /*
   * This too.
   */
  def hole[Ret](symbolId: scala.Int)(implicit holeEv: HoleEvidence[Ret]): Ret =
    holeEv emit symbolId

  trait IntOps {
    def +(that: Int): Int = IntPlus(IntOps.this, that)
    def value: scala.Int
  }

  // actual classes that provide lifting
  case class IntConst(i: scala.Int) extends IntOps {
    override def toString = s"$i"
    def value = i
  }

  case class IntPlus(l: IntOps, r: IntOps) extends IntOps {
    override def toString = s"($l + $r)"
    def value = 5
  }

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = IntConst(v)
  }

  implicit object HoleInt extends HoleEvidence[Int] {
    def emit(symbolId: scala.Int): Int = new IntConst(0) {
      override def value = throw new Exception("This value cannot be used.")
      override val toString = generateName(symbolId)
    }
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
