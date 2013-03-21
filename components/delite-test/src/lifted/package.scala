import ch.epfl.lamp.yinyang._
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
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.ScalaDSL", shallow = false, debug = false, rep = true)(block)
    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.ScalaDSL", shallow = false, debug = true, rep = true)(block)
    def optiML[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.OptiML", shallow = false, debug = false, rep = true, mainMethod = "mainDelite")(block)
    def optiMLDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.OptiML", shallow = false, debug = true, rep = true, mainMethod = "mainDelite")(block)

    def optiGraph[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.OptiGraph", shallow = false, debug = false, rep = true, mainMethod = "mainDelite")(block)
    def optiGraphDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = new YYTransformer[c.type, T](c, "lifted.OptiGraph", shallow = false, debug = true, rep = true, mainMethod = "mainDelite")(block)
  }

}
