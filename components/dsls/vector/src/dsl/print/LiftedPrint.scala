package dsl.print

import ch.epfl.lamp.mpde.api._

/** The int printing DSL */
trait PrintDSL extends CodeGenerator with base.LiftBase with MiniIntDSL with MiniUnitDSL with MiniPrintDSL with base.Interpret {

  var sb: StringBuffer = new StringBuffer()

  // hey, we can refine println -- but we don't need it right now
  def println(x: Any) = sb.append(s"scala.Predef.println(${x.toString});\n")
  def returns(x: Any) = sb.append(x.toString + ";\n") // added because we cannot test output

  def generateCode: String = {
    this.main()
    sb.toString
  }

  override def interpret(): Any = {
    this.main()
    println("Here the compiler should be invoked to compile and run the sb!")
    println(sb.toString)
    ()
  }

  def hole[T](variable: String): T = ???

}

trait PrintDSLInterpret extends base.LiftBase with MiniUnitDSL with MiniPrintDSL with base.Interpret {

  type Int = scala.Int
  implicit object LiftInt extends LiftEvidence[scala.Int, scala.Int] { def lift(i: Int) = i }

  // hey, we can refine println -- but we don't need it right now
  def println(x: Any) = scala.Predef.println(x.toString)
  def returns(x: Any) = x

  override def interpret(): Any = {
    this.main()
  }
}

trait MiniIntDSL extends base.LiftBase {

  type Int = IntOps

  trait IntOps {
    def +(that: Int): Int = (IntOps.this, that) match {
      case (IntConst(x), IntConst(y)) ⇒
        IntConst(x + y)
      case _ ⇒
        IntPlus(IntOps.this, that)
    }
  }

  // actual classes that provide lifting
  case class IntConst(i: scala.Int) extends IntOps { override def toString = s"$i" }
  case class IntPlus(l: IntOps, r: IntOps) extends IntOps { override def toString = s"$l + $r" }

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
  def returns(x: Any): Any
}
