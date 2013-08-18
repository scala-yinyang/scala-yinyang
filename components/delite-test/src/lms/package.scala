import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object lms {
  def lms[T](block: => T): T = macro implementations.lms[T]
  def lmsDebug[T](block: => T): T = macro implementations.lmsDebug[T]

  object implementations {
    def lms[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lms.ScalaDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false))(block)

    def lmsDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lms.ScalaDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false, "debug" -> 7))(block)
  }
}