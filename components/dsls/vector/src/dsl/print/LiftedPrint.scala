package dsl.print

import ch.epfl.yinyang.api._
import base._
import scala.collection._
import reflect.runtime.universe._

/** The basic int printing DSL. */
abstract class BasePrintDSL
  extends ScalaCompile with PrintCodeGenerator with CodeGenerator with MiniIntDSL
  with BooleanOps with Interpreted with BaseYinYang with Base {

  var sb: StringBuffer = new StringBuffer()

  // hey, we can refine println -- but we don't need it right now
  def println(x: Int): Int = {
    sb.append(s"scala.Predef.println(${x.toString});\n")
    IntConst(1)
  }

  override def reset(): Unit = {
    sb = new StringBuffer()
    holes.clear
  }

  def generateCode(className: String): String = {
    reset()
    val res = main()
    val distinctHoles = holes.distinct
    s"""
      class $className extends Function${distinctHoles.size}[${"Int, " * distinctHoles.size} Int] {
        def apply(${distinctHoles.map(y => y.toString + ": " + y.tpe.tpe.toString).mkString("", ",", "")}) = {
          ${sb.toString}
          ${res.toString}
        }
      }
    """
  }

  override def interpret[T: TypeTag](params: Any*): T = {
    if (compiledCode == null) {
      compiledCode = compile[T, () => T]
    }
    compiledCode.apply().asInstanceOf[T]
  }

  var compiledCode: () => Any = _
}

/**
 * The int printing DSL is FullyStaged, so no information is required for compile
 * time optimizations.
 */
abstract class PrintDSL extends BasePrintDSL with FullyStaged {}

/**
 * The int printing DSL extended with the <code>optimizingPrintln</code>
 * method to show how requiredHoles is used to signal that data is needed
 * for compile time optimizations.
 */
abstract class OptimizedPrintDSL extends BasePrintDSL {
  val recompileHoles = mutable.Set[scala.Int]()

  // x is needed for optimizations, so it is a required hole.
  def optimizingPrintln(x: Int): Int = {
    // imagine that we create a fancy specialized print here
    sb.append("scala.Predef.println(\"optimizing on: " + x.toString + "\");\n")
    x match {
      case Hole(tpe, id) =>
        recompileHoles += id
        IntConst(1)
      case _ =>
        IntConst(1)
    }
  }

  override def reset(): Unit = {
    recompileHoles.clear
    super.reset()
  }

  override def requiredHoles(symbols: List[Symbol]): List[Symbol] = {
    reset()
    main()

    recompileHoles.toList.map(symbols(_))
  }
}

trait PrintCodeGenerator { self: CodeGenerator =>
  trait BaseHole[T] {
    def tpe: TypeTag[T]
  }
  val holes: mutable.ArrayBuffer[BaseHole[_]] = new mutable.ArrayBuffer()
}

trait MiniIntDSL extends BaseYinYang { self: BooleanOps with PrintCodeGenerator =>

  type Int = IntOps

  trait IntOps {
    def +(that: Int): Int = IntPlus(IntOps.this, that)
    def -(that: Int): Int = IntPlus(IntOps.this, that)
    def value: scala.Int
    def __==(that: Int): Boolean = IntEq(IntOps.this, that)
    def __!=(that: Int): Boolean = IntEq(IntOps.this, that)
    def __hashCode(): Int = IntConst(value)
    def __##(): Int = IntConst(value)
  }

  // classes that provide lifting
  case class IntConst(i: scala.Int) extends IntOps {
    override def toString = s"$i"
    def value = i
  }

  case class IntPlus(l: IntOps, r: IntOps) extends IntOps {
    override def toString = s"($l + $r)"
    def value = 5
  }

  case class IntEq(l: IntOps, r: IntOps) extends BooleanOps {
    override def toString = s"$l == $r"
    def value = true
  }

  implicit object LiftInt extends LiftEvidence[scala.Int, Int] {
    def lift(v: scala.Int): Int = IntConst(v)
    def hole(tpe: TypeTag[scala.Int], symbolId: scala.Int): Int = {
      val h = Hole(tpe, symbolId)
      holes += h
      h
    }
  }

  case class Hole[T](tpe: TypeTag[T], symbolId: scala.Int) extends IntOps with BaseHole[T] {
    override def toString = "x" + symbolId
    def value = -1
  }

}

trait BooleanOps extends BaseYinYang { self: PrintCodeGenerator =>

  type Boolean = BooleanOps

  trait BooleanOps {
    def <(that: Boolean): Boolean = BooleanLess(BooleanOps.this, that)
    def value: scala.Boolean
  }

  // actual classes that provide lifting
  case class BooleanConst(i: scala.Boolean) extends BooleanOps {
    override def toString = s"$i"
    def value = i
  }

  case class BooleanLess(l: Boolean, r: Boolean) extends BooleanOps {
    override def toString = s"($l < $r)"
    def value = true
  }

  implicit object LiftBoolean extends LiftEvidence[scala.Boolean, Boolean] {
    def lift(v: scala.Boolean): Boolean = BooleanConst(v)
    def hole(tpe: TypeTag[scala.Boolean], symbolId: scala.Int): Boolean = {
      val h = BooleanHole(tpe, symbolId)
      holes += h
      h
    }
  }

  case class BooleanHole[T](tpe: TypeTag[T], symbolId: scala.Int) extends BaseHole[T] with BooleanOps {
    override def toString = "x" + symbolId
    def value = true
  }

}

trait MiniUnitDSL extends BaseYinYang {
  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: scala.Unit): Unit = ()
    def hole(tpe: TypeTag[scala.Unit], symbolId: scala.Int): Unit = {
      ()
    }
  }
}
