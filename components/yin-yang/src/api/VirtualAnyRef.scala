package ch.epfl.yinyang
package api

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Member method-based virtualization of the `AnyRef` API.
 *
 * This trait provides implementations of the infix methods
 * corresponding to the `AnyRef` API that delegate to virtualized
 * method calls on the first argument of the infix method.
 *
 * Example: When faced with an expression of the form `x.eq(y)`, the
 * `ch.epfl.yinyang.transformers.LanguageVirtualization` transformation (or the
 * `@virtualized` macro annotation) will generate a method call:
 * `infix_eq(x, y)`.  This method call will be bound to an
 * implementation based on normal rules of scoping.  If it binds to
 * the one in this trait, the corresponding macro will rewrite it to
 * `x.__eq(y)`.
 */
trait VirtualAnyRef extends VirtualAny {

  import VirtualAnyRef._

  // NOTE: Some of the signatures below have "by-val" arguments where
  // one would expect "by-name" arguments.  However, since these are
  // all macros the difference is irrelevant.  Furthermore, there's
  // currently a bug precluding the use of "by-name" parameters in
  // macros (See [[https://issues.scala-lang.org/browse/SI-5778
  // SI-5778]]).

  // Poor man's infix methods for `AnyRef` methods
  def infix_eq(x1: AnyRef, x2: AnyRef): Boolean = macro anyRef_eq
  def infix_ne(x1: AnyRef, x2: AnyRef): Boolean = macro anyRef_ne
  def infix_notify(x: AnyRef): Unit = macro anyRef_notify
  def infix_notifyAll(x: AnyRef): Unit = macro anyRef_notifyAll
  def infix_synchronized[T](x: AnyRef, body: T): T = macro anyRef_synchronized[T]
  def infix_wait(x: AnyRef): Unit = macro anyRef_wait0
  def infix_wait(x: AnyRef, timeout: Long): Unit = macro anyRef_wait1
  def infix_wait(x: AnyRef, timeout: Long, nanos: Int): Unit = macro anyRef_wait2
  def infix_clone(x: AnyRef): AnyRef = macro anyRef_clone
  def infix_finalize(x: AnyRef): Unit = macro anyRef_finalize
}

/**
 * VirtualAnyRef companion object containing macro implementations.
 */
private object VirtualAnyRef {

  def anyRef_eq(c: Context)(
    x1: c.Expr[AnyRef], x2: c.Expr[AnyRef]): c.Expr[Boolean] = {

    import c.universe._
    c.Expr(q"$x1.__eq($x2)")
  }

  def anyRef_ne(c: Context)(
    x1: c.Expr[AnyRef], x2: c.Expr[AnyRef]): c.Expr[Boolean] = {

    import c.universe._
    c.Expr(q"$x1.__ne($x2)")
  }

  def anyRef_notify(c: Context)(x: c.Expr[AnyRef]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__notify()")
  }

  def anyRef_notifyAll(c: Context)(x: c.Expr[AnyRef]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__notifyAll()")
  }

  def anyRef_synchronized[T](c: Context)(
    x: c.Expr[AnyRef], body: c.Expr[T]): c.Expr[T] = {

    import c.universe._
    c.Expr(q"$x.__synchronized($body)")
  }

  def anyRef_wait0(c: Context)(x: c.Expr[AnyRef]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__wait()")
  }

  def anyRef_wait1(c: Context)(
    x: c.Expr[AnyRef], timeout: c.Expr[Long]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__wait($timeout)")
  }

  def anyRef_wait2(c: Context)(
    x: c.Expr[AnyRef], timeout: c.Expr[Long],
    nanos: c.Expr[Int]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__wait($timeout, $nanos)")
  }

  def anyRef_clone(c: Context)(x: c.Expr[AnyRef]): c.Expr[AnyRef] = {

    import c.universe._
    c.Expr(q"$x.__clone()")
  }

  def anyRef_finalize(c: Context)(x: c.Expr[AnyRef]): c.Expr[Unit] = {

    import c.universe._
    c.Expr(q"$x.__finalize()")
  }
}
