package dsl.print

import ch.epfl.lamp.mpde._
import scala.collection.mutable.WeakHashMap
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxFactory

object `package` {

  object __compileStorage {

    private val map = WeakHashMap[String, (List[Any], () ⇒ Any)]()

    private def apply(key: String): (List[Any], () ⇒ Any) =
      map(key)

    private def initialized(dsl: String): Boolean =
      !map.get(dsl).isEmpty

    private def init(dsl: String, current: List[Any], program: () ⇒ Any): Unit =
      map.update(dsl, (current, program))

    def checkAndUpdate[T](dsl: String, current: List[Any], recompile: () ⇒ () ⇒ T): () ⇒ T = {
      if (!initialized(dsl)) {
        val program = recompile()
        init(dsl, current, program)
        program
      } else {
        val (previous, compiled) = apply(dsl)
        if (previous.length != current.length || (previous zip current exists { case (old, now) ⇒ old != now })) {
          val recompiled = recompile()
          map.update(dsl, (current, recompiled))
          recompiled
        } else {
          compiled.asInstanceOf[() ⇒ T]
        }
      }
    }

  }

  def liftPrint[T](block: ⇒ T): T = macro _liftPrint[T]
  def _liftPrint[T](c: Context)(block: c.Expr[T]): c.Expr[T] =
    new MPDETransformer[c.type, T](c, "dsl.print.PrintDSL")(block)

  // The only thing we declare here
  def println(x: Any) = ???

  def break(x: Int) = ???

}
