package ch.epfl.yinyang
package annotation

import ch.epfl.yinyang.transformers.LanguageVirtualization
import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

/** Annotation class for @virtualize macro annotation. */
final class virtualize extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro virtualize.impl
}

/** Companion object implementing @virtualize macro annotation. */
private object virtualize {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    /* Create a transformer for virtualization. */
    val transformer = new Virtualizer[c.type](c)

    /* The first element of `annottee` is the one actually carrying the
     * annotation.  The rest are owners/companions (class, method,
     * object, etc.), and we don't want to virtualize them.
     *
     * Also, for now, we don't virtualize annotated type, class or
     * method parameters (this may change in the future).
     */
    val inputs = annottees.map(_.tree).toList
    val (annottee, rest) = inputs match {
      case (_: ValDef | _: TypeDef) :: as => (None, as)
      case a :: as                        => (Some(a), as)
      case Nil                            => (None, Nil)
    }

    /* Virtualize the annottee. */
    val expandees = annottee match {
      case Some(a) => transformer.virtualize(a)._1 :: rest
      case None    => rest
    }

    c.Expr[Any](Block(expandees, Literal(Constant(()))))
  }

  private final class Virtualizer[C <: Context](val c: C)
    extends LanguageVirtualization {
    type Ctx = C
    val debugLevel = 0
  }
}
