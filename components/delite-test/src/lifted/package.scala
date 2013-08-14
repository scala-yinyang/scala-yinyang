import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object lifted {
  def lift[T](block: => T): T = macro implementations.lift[T]
  def liftDebug[T](block: => T): T = macro implementations.liftDebug[T]
  def optiML[T](block: => T): T = macro implementations.optiML[T]
  def optiMLDebug[T](block: => T): T = macro implementations.optiMLDebug[T]
  def optiGraph[T](block: => T): T = macro implementations.optiGraph[T]
  def optiGraphDebug[T](block: => T): T = macro implementations.optiGraphDebug[T]
  def optiGraphAnalysis[T](block: => T): T = macro implementations.optiGraphAnalysis[T]

  object implementations {
    def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.ScalaDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false))(block)

    def liftDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.ScalaDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false, "debug" -> true))(block)

    def optiML[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.OptiML",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false, "mainMethod" -> "mainDelite"))(block)

    def optiMLDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.OptiML",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false,
          "mainMethod" -> "mainDelite",
          "debug" -> true))(block)

    def optiGraph[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.OptiGraph",
        new RepTransformer[c.type](c),
        None,
        Map(
          "shallow" -> false,
          "mainMethod" -> "mainDelite",
          "debug" -> false))(block)

    def optiGraphDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.OptiML",
        new RepTransformer[c.type](c),
        None,
        Map(
          "shallow" -> false,
          "mainMethod" -> "mainDelite",
          "debug" -> true))(block)

    def optiGraphAnalysis[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "lifted.OptiGraph",
        new RepTransformer[c.type](c),
        None,
        Map(
          "shallow" -> true,
          "mainMethod" -> "mainDelite",
          "debug" -> true))(block)
  }

}
