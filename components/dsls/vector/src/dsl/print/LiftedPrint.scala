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
 * The unstaged version of the int printing DSL declares that it doesn't
 * require any variables for optimizations, so it will be generated at
 * compile time.
 */
abstract class UnstagedPrintDSL extends BasePrintDSL with FullyUnstaged {}

/**
 * The optimizing version of the int printing DSL uses compilationVars to
 * signal which holes are needed for optimizations, e.g. those that appear as
 * arguments to the <code>optimizingPrintln</code> method. If there are no such
 * holes in the DSL program, it will be generated at compile time.
 */
abstract class OptimizedPrintDSL extends BasePrintDSL {
  val compilationHoles = mutable.Set[scala.Int]()

  // x is needed for optimizations, so it is a required hole.
  def optimizingPrintln(x: Int): Int = {
    // imagine that we create a fancy specialized print here
    sb.append("scala.Predef.println(\"optimizing on: " + x.toString + "\");\n")
    x match {
      case Hole(tpe, id) =>
        compilationHoles += id
        IntConst(1)
      case _ =>
        IntConst(1)
    }
  }

  override def reset(): Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[(Symbol, Guard)] = {
    reset()
    main()

    compilationHoles.toList.map(i => (symbols(i), Guard.defaultGuard))
  }
}

/**
 * The even-odd optimizing version of the int printing DSL offers two versions
 * of printing code, one for even and one for odd integers. It shows how to use
 * custom guards to define when recompilation is triggered, and how dynamic
 * variables allow both optimizations on a concrete value and generating code
 * containing the variable.
 */
abstract class EvenOddOptimizedPrintDSL extends BasePrintDSL {
  val compilationHoles = mutable.Map[scala.Int, Guard]()

  def evenOddPrintln(x: Int): Int = {
    x match {
      case Hole(tpe, id) =>
        // Retrieve previously stored guard.
        val guard = compilationHoles.getOrElse(id, Guard.always_true)
        // Compose previous guard with custom guard function, indicating with
        // the Boolean that this variable is dynamic, so it will be
        // transformed to a liftedHole.
        compilationHoles.put(id, guard.and(Guard.custom("t1.asInstanceOf[scala.Int] % 2 == t2.asInstanceOf[scala.Int] % 2", true)))
        IntConst(1)
      case IntConst(value, hole) =>
        // The dynamic variable contains both a value for code generation time
        // and the hole that can be used in the generated code.
        sb.append("scala.Predef.println(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + (hole match {
          case None    => value + "\""
          case Some(h) => "\" + " + h.toString
        }) + ");\n")
        IntConst(1)
    }
  }

  override def reset(): Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[(Symbol, Guard)] = {
    reset()
    main()

    compilationHoles.toList.map(x => (symbols(x._1), x._2))
  }
}

/**
 * The fully staged version of the int printing DSL declares that it requires
 * all variables for optimizations (even if we don't use them in this example),
 * so if the DSL program has at least one hole, it cannot be generated at
 * compile time.
 */
abstract class StagedPrintDSL extends BasePrintDSL with FullyStaged {}

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
  case class IntConst(i: scala.Int, h: Option[Int] = None) extends IntOps {
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
    // The EvenOddOptimizedPrintDSL uses dynamic Int compilation vars.
    override def lift(v: scala.Int, h: Option[Int] = None): Int = IntConst(v, h)

    def lift(v: scala.Int): Int = IntConst(v, None)
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
