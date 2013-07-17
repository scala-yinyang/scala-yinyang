package dsl.print

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  def liftPrint[T](block: => T): T = macro _liftPrint[T]
  def liftPrintDebug[T](block: => T): T = macro _liftPrintDebug[T]
  def liftOptimizedPrint[T](block: => T): T = macro _liftOptimizedPrint[T]

  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.PrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  def _liftPrintDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.PrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false, "debug" -> 1))(block)

  def _liftOptimizedPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.OptimizedPrintDSL",
      new PolyTransformer[c.type](c),
      None,
      Map("shallow" -> false))(block)

  // Shallow embedding:
  def println(x: Int): Int = { scala.Predef.println("shallow: " + x); x }
  def optimizingPrintln(x: Int): Int = { scala.Predef.println("shallow optimizing: " + x); x }

  // No shallow embedding necessary
  //  def println(x: Int): Int = ???
  //  def optimizingPrintln(x: Int): Int = ???
}
