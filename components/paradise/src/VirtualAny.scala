package ch.epfl.yinyang
package api

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Member method-based virtualization of the `Any` API.
 *
 * This trait provides implementations of the infix methods
 * corresponding to the `Any` API that delegate to virtualized method
 * calls on the first argument of the infix method.
 *
 * Example: When faced with an expression of the form `x == y`, the
 * `ch.epfl.yinyang.transformers.LanguageVirtualization`
 * transformation (or the `@virtualized` macro annotation)
 * will generate a method call:
 * `infix_==(x, y)`.  This method call will be bound to an
 * implementation based on normal rules of scoping.  If it binds to
 * the one in this trait, the corresponding macro will rewrite it to
 * `x.__==(y)`.
 */
trait VirtualAny {

  import VirtualAny._

  // Poor man's infix methods for `Any` methods
  def $infix_==(x1: Any, x2: Any): Any = macro any_==
  def $infix_!=(x1: Any, x2: Any): Any = macro any_!=
  def $infix_##(x: Any): Any = macro any_##
  def $infix_equals(x1: Any, x2: Any): Any = macro any_equals
  def $infix_hashCode(x: Any): Any = macro any_hashCode
  def $infix_asInstanceOf[T](x: Any): Any = macro any_asInstanceOf[T]
  def $infix_isInstanceOf[T](x: Any): Any = macro any_isInstanceOf[T]
  def $infix_toString(x: Any): Any = macro any_toString
  def $infix_getClass(x: Any): Any = macro any_getClass
}

/* VirtualAny companion object containing macro implementations. */
private object VirtualAny {

  def any_==(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x1.__==($x2)")
  }

  def any_!=(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x1.__!=($x2)")
  }

  def any_##(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x.__##()")
  }

  def any_equals(c: Context)(
    x1: c.Expr[Any], x2: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x1.__equals($x2)")
  }

  def any_hashCode(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x.__hashCode()")
  }

  def any_asInstanceOf[T](c: Context)(x: c.Expr[Any])(
    implicit tt: c.WeakTypeTag[T]): c.Expr[Any] = {

    import c.universe._
    c.Expr[Any](q"$x.__asInstanceOf[${tt.tpe}]")
  }

  def any_isInstanceOf[T](c: Context)(x: c.Expr[Any])(
    implicit tt: c.WeakTypeTag[T]): c.Expr[Boolean] = {

    import c.universe._
    c.Expr[Boolean](q"$x.__isInstanceOf[${tt.tpe}]")
  }

  def any_toString(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x.__toString()")
  }

  def any_getClass(c: Context)(x: c.Expr[Any]): c.Expr[Any] = {

    import c.universe._
    c.Expr(q"$x.__getClass()")
  }
}
