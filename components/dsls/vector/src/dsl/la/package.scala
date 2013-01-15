package dsl

import ch.epfl.lamp.mpde._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object la {

  // TODO should not return Unit but a value
  def laLift[T](block: ⇒ T): Unit = macro lift[T]

  def laDebug[T](block: ⇒ T): Unit = macro liftDebug[T]

  def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "VectorDSL")(block)

  def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "VectorDSL", debug = true)(block)

}
