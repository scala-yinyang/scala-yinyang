package dsl.print

import ch.epfl.yinyang._
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  def liftPrintDebug[T](block: => T): T = macro _liftPrintDebug[T]
  def liftPrint[T](block: => T): T = macro _liftPrint[T]

  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.PrintDSL",
      new PolyTransformer[c.type](c),
      Map("shallow" -> false))(block)

  def _liftPrintDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c)(
      "dsl.print.PrintDSL",
      new PolyTransformer[c.type](c),
      Map("shallow" -> false, "debug" -> true))(block)

  // The only thing we declare here
  def println(x: Any): Int = ???

  def break(x: Int): Int = ???

}
