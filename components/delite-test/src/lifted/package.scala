import ch.epfl.lamp.mpde._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object lifted {
  def lift[T](block: ⇒ T): T = macro implementations.lift[T]
  def liftDebug[T](block: ⇒ T): T = macro implementations.liftDebug[T]
  def optiML[T](block: ⇒ T): T = macro implementations.optiML[T]

  object implementations {
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.ScalaDSL", debug = false, rep = true)(block)
    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.ScalaDSL", debug = true, rep = true)(block)
    def optiML[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.OptiML", debug = false, rep = true)(block)
  }

}
