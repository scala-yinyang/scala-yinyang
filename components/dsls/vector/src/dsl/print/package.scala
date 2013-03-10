package dsl.print

import ch.epfl.lamp.mpde._
import ch.epfl.lamp.mpde.api.CompiledStorage
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  val __compileStorage = CompiledStorage

  def liftPrint[T](block: ⇒ T): T = macro _liftPrint[T]
  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    new MPDETransformer[c.type, T](c, "dsl.print.PrintDSL")(block)

  // The only thing we declare here
  def println(x: Any) = ???

  def break(x: Int) = ???

}
