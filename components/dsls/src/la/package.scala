package dsl

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object la {

  def la[T](block: => T): Unit = macro implementations.liftRep[T]

  object implementations {
    def liftRep[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
      YYTransformer[c.type, T](c)(
        "dsl.la.rep.VectorDSL",
        new GenericTypeTransformer[c.type](c) {
          override val IRType = "R"
        },
        None, None,
        Map(
          "shallow" -> false,
          "virtualizeFunctions" -> true,
          "virtualizeVal" -> true,
          "debug" -> 3,
          "featureAnalysing" -> false,
          "ascriptionTransforming" -> false))(block)
  }
}
