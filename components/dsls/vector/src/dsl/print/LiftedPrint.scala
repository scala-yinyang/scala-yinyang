package dsl.print

import ch.epfl.yinyang.api._
import base._
import scala.collection._
import scala.collection.immutable.Set
import reflect.runtime.universe._

/** The basic int printing DSL. */
abstract class BasePrintDSL
  extends ScalaCompile with PrintCodeGenerator with CodeGenerator with MiniIntDSL
  with BooleanOps with MiniUnitDSL with Interpreted with BaseYinYang with Base {

  var sb: StringBuffer = new StringBuffer()

  def print(x: Int): Unit = {
    sb.append(s"""scala.Predef.print(${x.toString} + " ");""" + "\n")
  }

  override def reset(): scala.Unit = {
    sb = new StringBuffer()
    holes.clear
  }

  def generateCode(className: String, unstableHoleIds: Set[scala.Int]): String = {
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
      compiledCode = compile[T, () => T]()
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
 * arguments to the <code>optimizingPrint</code> method. If there are no such
 * holes in the DSL program, it will be generated at compile time.
 */
abstract class OptimizedPrintDSL extends BasePrintDSL {
  val compilationHoles = mutable.Set[scala.Int]()

  // x is needed for optimizations, so it is a compilation variable.
  def optimizingPrint(x: Int): Unit = {
    sb.append("scala.Predef.print(\"optimizing on: " + x.toString + " \");\n")
    x match {
      case Hole(tpe, id) =>
        compilationHoles += id
      case _ =>
    }
  }

  override def reset(): scala.Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[VarType] = {
    reset()
    main()

    // Since we want to generate a new version of the print code for each value
    // we mark the holes encountered in optimizingPrint as DefaultCompVar.
    List.range(0, symbols.length).map(i => if (compilationHoles.contains(i)) DefaultCompVar else NonCompVar())
  }
}

/**
 * The even-odd optimizing version of the int printing DSL offers two versions
 * of printing code, one for even and one for odd integers. It shows how to use
 * dynamic compilation variables with custom guards to define when
 * recompilation is triggered. The first step is to mark the variables as
 * RequiredDynamicCompVar. Then we define the custom guard function, which
 * returns whether two values t1 and t2 are equivalent with respect to
 * compilation or whether the new one triggers recompilation.
 *
 * Then, the MiniIntDSL was extended so that the LiftInt evidence implements the
 * second version of the lift function:
 *   override def lift(v: scala.Int, h: Option[Int] = None): Int = IntConst(v, h)
 * The IntConst node now also has a second argument, so it can carry both the
 * value indicating which parity version to use, and the hole that will be used
 * in the generated code.
 */
abstract class EvenOddOptimizedPrintDSL extends BasePrintDSL {
  // Although we only use one VarType here, we store and compose them using and
  // as would be done in the general case.
  val compilationHoles = mutable.Map[scala.Int, VarType]()

  def evenOddPrint(x: Int): Unit = {
    x match {
      case Hole(tpe, id) =>
        // Retrieve previously stored type or the neutral type.
        val varType: VarType = compilationHoles.getOrElse(id, NonCompVar())
        // Compose previous type with the new type:
        // required dynamic type with custom guard function.
        compilationHoles.put(id, RequiredDynamicCompVar(Guard.custom("t1.asInstanceOf[scala.Int] % 2 == t2.asInstanceOf[scala.Int] % 2")).and(varType))
      case IntConst(value) =>
        sb.append("scala.Predef.print(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + value + " \");\n")
      case IntMixed(value, hole) =>
        // The dynamic variable contains both a value for code generation time
        // and the hole that can be used in the generated code.
        sb.append("scala.Predef.print(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + "\" + " + hole.toString + " + \" \");\n")
    }
  }

  override def reset(): scala.Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[VarType] = {
    reset()
    main()

    List.range(0, symbols.length).map(i => compilationHoles.getOrElse(i, NonCompVar()))
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
  val compilationHoles = mutable.Set[scala.Int]()

  def returningIncrementedPrint(x: Int): Int = {
    sb.append("scala.Predef.print(\"inc: " + x.toString + " \");\n " + x.toString + " + 1;\n")
    x match {
      case Hole(tpe, id) =>
        compilationHoles += id
      case _ =>
    }
    // TODO this hack doesn't work for nesting calls, need AST instead of manual emission to buffer,
    // but how to get AST from type-checking main function?
    CodeGeneratingStatement()
  }

  override def reset(): Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[VarType] = {
    reset()
    main()

    List.range(0, symbols.length).map(i => if (compilationHoles.contains(i)) DefaultCompVar else NonCompVar())
  }
}

abstract class VarTypePrintDSL extends BasePrintDSL {
  val compilationHoles = mutable.Map[scala.Int, VarType]()

  // GenerateCode sets this variable to contain the decision using the runtime
  // statistics about what to stage.
  var unstableHoleIds: Set[scala.Int] = Set()

  // Remember which optional variables ended up as static in compilationVars.
  var optionalStatic: Set[scala.Int] = Set()

  def reqDynamicPrint(x: Int): Unit = {
    // compilationVars collection phase:
    x match {
      case Hole(tpe, id) =>
        val varType: VarType = compilationHoles.getOrElse(id, NonCompVar())
        compilationHoles.put(id, RequiredDynamicCompVar(Guard.custom("t1.asInstanceOf[scala.Int] % 2 == t2.asInstanceOf[scala.Int] % 2")).and(varType))
      case _ =>
    }

    // code generation phase:
    x match {
      case IntConst(value) => sb.append("scala.Predef.print(\"" + (value % 2 match {
        case 0 => "Even: "
        case 1 => "Odd: "
      }) + "\" + " + value + " + \" \");\n")
      case IntMixed(value, hole) => sb.append("scala.Predef.print(\"" + (value % 2 match {
        case 0 => "Even: "
        case 1 => "Odd: "
      }) + "\" + " + hole + " + \" \");\n")
      case _ =>
    }
  }

  def reqStaticPrint(x: Int): Unit = {
    // compilationVars collection phase:
    x match {
      case Hole(tpe, id) =>
        val varType: VarType = compilationHoles.getOrElse(id, NonCompVar())
        compilationHoles.put(id, RequiredStaticCompVar(Guard.defaultGuard).and(varType))
      case _ =>
    }

    // code generation phase:
    x match {
      case x: IntValue => sb.append("scala.Predef.print(\"" + (x.i % 2 match {
        case 0 => "Even: "
        case 1 => "Odd: "
      }) + "\" + " + x.i + " + \" \");\n")
      case _ =>
    }
  }

  def optionalDynamicPrint(x: Int): Unit = {
    // compilationVars collection phase:
    x match {
      case Hole(tpe, id) =>
        val varType: VarType = compilationHoles.getOrElse(id, NonCompVar())
        compilationHoles.put(id, OptionalDynamicCompVar(Guard.custom("t1.asInstanceOf[scala.Int] % 2 == t2.asInstanceOf[scala.Int] % 2")).and(varType))
      case _ =>
    }

    // code generation phase:
    x match {
      case IntConst(value) => sb.append("scala.Predef.print(\"" + (value % 2 match {
        case 0 => "Even: "
        case 1 => "Odd: "
      }) + "\" + " + value + " + \" \");\n")
      case IntMixed(_, h @ Hole(tpe, id)) if unstableHoleIds.contains(id) =>
        sb.append(s"""if ($h % 2 == 0) scala.Predef.print("Even: " + $h + " ");""")
        sb.append("\nelse scala.Predef.print(\"Odd: \" + " + h + " + \" \");\n")
      case IntMixed(value, hole) =>
        sb.append("scala.Predef.print(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + "\" + " + hole + " + \" \");\n")
      case _ =>
    }
  }

  def optionalStaticPrint(x: Int): Unit = {
    // compilationVars collection phase:
    x match {
      case Hole(tpe, id) =>
        val varType: VarType = compilationHoles.getOrElse(id, NonCompVar())
        compilationHoles.put(id, OptionalStaticCompVar(Guard.defaultGuard).and(varType))
      case _ =>
    }

    // code generation phase:
    x match {
      case IntConst(value) => sb.append("scala.Predef.print(\"" + (value % 2 match {
        case 0 => "Even: "
        case 1 => "Odd: "
      }) + "\" + " + value + " + \" \");\n")
      case IntMixed(_, hole @ Hole(_, id)) if unstableHoleIds.contains(id) =>
        sb.append(s"""if ($hole % 2 == 0) scala.Predef.print("Even: " + $hole + " ");""")
        sb.append("\nelse scala.Predef.print(\"Odd: \" + " + hole + " + \" \");\n")
      case IntMixed(value, Hole(_, id)) if optionalStatic.contains(id) =>
        sb.append("scala.Predef.print(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + "\" + " + value + " + \" \");\n")
      case IntMixed(value, hole) =>
        sb.append("scala.Predef.print(\"" + (value % 2 match {
          case 0 => "Even: "
          case 1 => "Odd: "
        }) + "\" + " + hole + " + \" \");\n")
      case _ =>
    }
  }

  override def reset(): scala.Unit = {
    compilationHoles.clear
    super.reset()
  }

  override def compilationVars(symbols: List[Symbol]): List[VarType] = {
    reset()
    main()

    val varTypes = List.range(0, symbols.length).map(i => compilationHoles.getOrElse(i, NonCompVar()))
    optionalStatic = varTypes.zipWithIndex.collect({ case (_: OptionalStaticCompVar, i) => i }).toSet

    varTypes
  }

  override def generateCode(className: String, unstableHoleIds: Set[scala.Int]): String = {
    this.unstableHoleIds = unstableHoleIds
    super.generateCode(className, unstableHoleIds)
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

  trait IntValue extends IntOps {
    def i: scala.Int
  }

  // classes that provide lifting
  case class IntConst(i: scala.Int) extends IntValue {
    override def toString = s"$i"
    def value = i
  }

  case class IntMixed(i: scala.Int, hole: Int) extends IntValue {
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
    // The EvenOddOptimizedPrintDSL uses dynamic Int compilation vars.
    override def mixed(v: scala.Int, hole: Int): Int = IntMixed(v, hole)

    def lift(v: scala.Int): Int = IntConst(v)
    def hole(tpe: TypeTag[scala.Int], holeId: scala.Int): Int = {
      val h = Hole(tpe, holeId)
      holes += h
      h
    }
  }

  case class Hole[T](tpe: TypeTag[T], holeId: scala.Int) extends IntOps with BaseHole[T] {
    override def toString = "x" + holeId
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
