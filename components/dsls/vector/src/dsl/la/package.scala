package dsl

import ch.epfl.lamp.mpde._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object la {

  // TODO (Vojin) should not return Unit but a value
  def laLift[T](block: ⇒ T): Unit = macro implementations.lift[T]
  def laDebug[T](block: ⇒ T): Unit = macro implementations.liftDebug[T]

  object implementations {
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "dsl.la.norep.VectorDSL")(block)
    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "dsl.la.norep.VectorDSL", debug = true)(block)
  }
}
