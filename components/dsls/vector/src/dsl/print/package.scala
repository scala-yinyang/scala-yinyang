package dsl.print

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  def liftUnstagedPrint[T](block: => T): T = macro _liftUnstagedPrint[T]
  def liftOptimizedPrint[T](block: => T): T = macro _liftOptimizedPrint[T]
  def liftEvenOddOptimizedPrint[T](block: => T): T = macro _liftEvenOddOptimizedPrint[T]
  def liftStagedPrint[T](block: => T): T = macro _liftStagedPrint[T]
  def liftReturningPrint[T](block: => T): T = macro _liftReturningPrint[T]
  def liftVarTypeStab10Print[T](block: => T): T = macro _liftVarTypeStab10Print[T]
  def liftVarTypeInitiallyUnstableStab10Print[T](block: => T): T = macro _liftVarTypeInitiallyUnstableStab10Print[T]

  def _liftUnstagedPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.UnstagedPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftOptimizedPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.OptimizedPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftEvenOddOptimizedPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.EvenOddOptimizedPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftStagedPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.StagedPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftReturningPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.ReturningPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftVarTypeStab10Print[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.VarTypePrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false, "minimumCountToStabilize" -> 10))(block)

  def _liftVarTypeInitiallyUnstableStab10Print[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.VarTypePrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false, "optionalInitiallyStable" -> false, "minimumCountToStabilize" -> 10))(block)

  // Shallow embedding:
  def print(x: Int): Unit = scala.Predef.print("shallow: " + x + " ")
  def optimizingPrint(x: Int): Unit = scala.Predef.print("shallow optimizing: " + x + " ")
  def returningIncrementedPrint(x: Int): Int = { scala.Predef.print("inc: " + x + " "); x + 1 }

  // Shallow embedding not mandatory:
  def evenOddPrint(x: Int): Unit = ???

  // VarType DSL without shallow embedding
  def optionalStaticPrint(x: Int): Unit = ???
  def optionalDynamicPrint(x: Int): Unit = ???
  def reqStaticPrint(x: Int): Unit = ???
  def reqDynamicPrint(x: Int): Unit = ???
}
