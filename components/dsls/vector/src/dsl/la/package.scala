package dsl

import ch.epfl.yinyang._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object la {

  def laLiftRep[T](block: => T): Unit = macro implementations.liftRep[T]
  def laDebugRep[T](block: => T): Unit = macro implementations.liftDebugRep[T]
  def laLiftNoRep[T](block: => T): Unit = macro implementations.liftNoRep[T]
  def laDebugNoRep[T](block: => T): Unit = macro implementations.liftDebugNoRep[T]

  object implementations {
    def liftRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "dsl.la.rep.VectorDSL",
        new LMSTransformer[c.type](c),
        Map("shallow" -> false, "debug" -> false))(block)

    def liftDebugRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "dsl.la.rep.VectorDSL",
        new LMSTransformer[c.type](c),
        Map("shallow" -> false, "debug" -> true))(block)

    def liftNoRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "dsl.la.norep.VectorDSL",
        new PolyTransformer[c.type](c),
        Map("shallow" -> false, "debug" -> false))(block)

    def liftDebugNoRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "dsl.la.norep.VectorDSL",
        new PolyTransformer[c.type](c),
        Map("shallow" -> false, "debug" -> true))(block)
  }
}
