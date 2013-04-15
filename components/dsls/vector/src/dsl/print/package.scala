package dsl.print

import ch.epfl.lamp.yinyang._
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  def liftPrintDebug[T](block: => T): T = macro _liftPrintDebug[T]
  def liftPrint[T](block: => T): T = macro _liftPrint[T]
  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c, "dsl.print.PrintDSL", shallow = true)(block)
  def _liftPrintDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    YYTransformer[c.type, T](c, "dsl.print.PrintDSL",
      shallow = true,
      debug = true,
      rep = false)(block)

  // The only thing we declare here
  def println(x: Any) = ???

  def break(x: Int) = ???

}
