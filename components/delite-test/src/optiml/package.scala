package ch.epfl.lamp

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.language.experimental.macros
import scala.reflect.macros.Context

package object optiml extends VirtualizedDSL {
  def optiML[T](block: => T): T = macro implementations.optiML[T]
  def optiMLDebug[T](block: => T): T = macro implementations.optiMLDebug[T]

  object implementations {
    def optiML[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "ch.epfl.lamp.optiml.OptiMLDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false, "featureAnalysing" -> false, "ascriptionTransforming" -> false, "mainMethod" -> "mainDelite"))(block)

    def optiMLDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "ch.epfl.lamp.optiml.OptiMLDSL",
        new RepTransformer[c.type](c),
        None,
        Map("shallow" -> false, "debug" -> 7, "featureAnalysing" -> false, "ascriptionTransforming" -> false, "mainMethod" -> "mainDelite"))(block)
  }

}

// this is an unfortunate hack for scala virtualized
trait VirtualizedDSL {
  def __whileDo(c: => Boolean, b: => Unit): Unit = while (c) b
}