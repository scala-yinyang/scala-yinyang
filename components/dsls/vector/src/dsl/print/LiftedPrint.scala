package dsl.print

import ch.epfl.yinyang.api._
import base._
import scala.collection._
import reflect.runtime.universe._

/** The basic int printing DSL. */
abstract class BasePrintDSL
  extends ScalaCompile with PrintCodeGenerator with CodeGenerator with MiniIntDSL
  with BooleanOps with MiniUnitDSL with Interpreted with BaseYinYang with Base {

  var sb: StringBuffer = new StringBuffer()

  // hey, we can refine println -- but we don't need it right now
  def println(x: Int): Unit = {
    sb.append(s"scala.Predef.println(${x.toString});\n")
  }

  override def reset(): scala.Unit = {
    sb = new StringBuffer()
    holes.clear
  }

  def generateCode(className: String): String = {
    reset()
    val res = main()
    val retType = res match {
      case _: Int     => "Int" // scala.Int
      case _: Boolean => "Boolean"
      case _: Unit    => "Unit"
    }
    val distinctHoles = holes.distinct
    s"""
      class $className extends Function${distinctHoles.size}[${"Int, " * distinctHoles.size} $retType] {
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
 * The unstaged version of the int printing DSL declares that it doesn't
 * require any variables for optimizations, so it will be generated at
 * compile time.
 */
abstract class UnstagedPrintDSL extends BasePrintDSL with FullyUnstaged {}

/**
 * The optimizing version of the int printing DSL uses requiredHoles to signal
 * which holes are needed for optimizations, e.g. those that appear as
 * arguments to the <code>optimizingPrintln</code> method. If there are no such
 * holes in the DSL program, it will be generated at compile time.
 */
abstract class OptimizedPrintDSL extends BasePrintDSL {
  val recompileHoles = mutable.Set[scala.Int]()

  // x is needed for optimizations, so it is a required hole.
  def optimizingPrintln(x: Int): Unit = {
    // imagine that we create a fancy specialized print here
    sb.append("scala.Predef.println(\"optimizing on: " + x.toString + "\");\n")
    x match {
      case Hole(tpe, id) =>
        recompileHoles += id
      case _ =>
    }
  }

  override def reset(): scala.Unit = {
    recompileHoles.clear
    super.reset()
  }

  override def requiredHoles(symbols: List[Symbol]): List[Symbol] = {
    reset()
    main()

    recompileHoles.toList.map(symbols(_))
  }
}

/**
 * The fully staged version of the int printing DSL declares that it requires
 * all variables for optimizations (even if we don't use them in this example),
 * so if the DSL program has at least one hole, it cannot be generated at
 * compile time.
 */
abstract class StagedPrintDSL extends BasePrintDSL with FullyStaged {}

abstract class ReturningPrintDSL extends BasePrintDSL {
  val recompileHoles = mutable.Set[scala.Int]()

  def returningIncrementedPrintln(x: Int): Int = {
    sb.append("scala.Predef.println(\"inc: " + x.toString + "\");\n " + x.toString + " + 1;\n")
    x match {
      case Hole(tpe, id) =>
        recompileHoles += id
      case _ =>
    }
    // TODO this hack doesn't work for nesting calls, need AST instead of manual emission to buffer,
    // but how to get AST from type-checking main function?
    CodeGeneratingStatement()
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

  case class CodeGeneratingStatement extends IntOps {
    // This statement adds its code directly to the StringBuffer, so if it's in
    // result position, we don't want to add it again.
    override def toString = ""
    def value = -42
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
  type Unit = scala.Unit

  implicit object LiftUnit extends LiftEvidence[scala.Unit, Unit] {
    def lift(v: scala.Unit): Unit = ()
    def hole(tpe: TypeTag[scala.Unit], symbolId: scala.Int): Unit = {
      ()
    }
  }
}
