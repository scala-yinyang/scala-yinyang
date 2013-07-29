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

  // Shallow embedding:
  def println(x: Int): Int = { scala.Predef.println("shallow: " + x); x }
  def optimizingPrintln(x: Int): Int = { scala.Predef.println("shallow optimizing: " + x); x }

  // Shallow embedding not mandatory:
  def evenOddPrintln(x: Int): Int = ???
}
