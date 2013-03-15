import ch.epfl.lamp.mpde._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object lifted {
  def lift[T](block: ⇒ T): T = macro implementations.lift[T]
  def liftDebug[T](block: ⇒ T): T = macro implementations.liftDebug[T]
  def optiML[T](block: ⇒ T): T = macro implementations.optiML[T]
  def optiMLDebug[T](block: ⇒ T): T = macro implementations.optiMLDebug[T]
  def optiGraph[T](block: ⇒ T): T = macro implementations.optiGraph[T]
  def optiGraphDebug[T](block: ⇒ T): T = macro implementations.optiGraphDebug[T]

  object implementations {
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.ScalaDSL", debug = false, rep = true)(block)
    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.ScalaDSL", debug = true, rep = true)(block)
    def optiML[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.OptiML", debug = false, rep = true)(block)
    def optiMLDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.OptiML", debug = true, rep = true)(block)

    def optiGraph[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.OptiGraph", debug = false, rep = true)(block)
    def optiGraphDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new MPDETransformer[c.type, T](c, "lifted.OptiGraph", debug = true, rep = true)(block)
  }

}
