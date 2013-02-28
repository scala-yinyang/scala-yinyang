package dsl

import ch.epfl.lamp.yinyang._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object la {

  def laLift[T](block: ⇒ T): Unit = macro implementations.lift[T]
  def laDebug[T](block: ⇒ T): Unit = macro implementations.liftDebug[T]

  def laLiftRep[T](block: ⇒ T): Unit = macro implementations.liftRep[T]
  def laDebugRep[T](block: ⇒ T): Unit = macro implementations.liftDebugRep[T]
  def laLiftNoRep[T](block: ⇒ T): Unit = macro implementations.liftNoRep[T]
  def laDebugNoRep[T](block: ⇒ T): Unit = macro implementations.liftDebugNoRep[T]

  object implementations {
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.norep.VectorDSL")(block)
    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.norep.VectorDSL", debug = true)(block)

    def liftRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.rep.VectorDSL", debug = false, rep = true)(block)
    def liftDebugRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.rep.VectorDSL", debug = true, rep = true)(block)
    def liftNoRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.norep.VectorDSL", debug = false, rep = false)(block)
    def liftDebugNoRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "dsl.la.norep.VectorDSL", debug = true, rep = false)(block)
  }
}
