// package ch.epfl.yinyang

// import ch.epfl.yinyang._
// import ch.epfl.yinyang.transformers._
// import language.experimental.macros
// import scala.reflect.macros.Context

// /**
//  * Default implementation of virtualized Scala control structures.
//  *
//  * This trait is adapted from the [[scala.EmbeddedControls]] trait in
//  * Scala Virtualized.  See also
//  * [[https://raw.github.com/namin/scala/topic-virt/src/library/scala/EmbeddedControls.scala]]
//  *
//  * The `EmbeddedControls` trait provides method definitions where
//  * calls to the methods are treated by the compiler in a special way.
//  * The reason to express these calls as methods is to give embedded
//  * DSLs a chance to provide their own definitions and thereby override
//  * the standard interpretation of the compiler.
//  *
//  * Example: When faced with an `if` construct, the `@virtualized`
//  * macro annotation will generate a method call:
//  * `__ifThenElse(cond, thenp, elsep)`
//  *
//  * This method call will be bound to an implementation based on normal
//  * rules of scoping.  If it binds to the standard one in this trait,
//  * the corresponding macro will replace it by an `If` tree node. If
//  * not, the call will be left as it is and a staging or interpreting
//  * DSL can take over.
//  *
//  * @NOTE: This is experimental.
//  *        None of the above will happen unless you annotate your code
//  *        with `@virtualize`.
//  */
// trait EmbeddedControls {

//   import EmbeddedControls._

//   // NOTE: Some of the signatures below have "by-val" arguments where
//   // one would expect "by-name" arguments.  However, since these are
//   // all macros the difference is irrelevant.  Furthermore, there's
//   // currently a bug precluding the use of "by-name" parameters in
//   // macros (See [[https://issues.scala-lang.org/browse/SI-5778
//   // SI-5778]]).

//   def __ifThenElse[T](cond: Boolean, thenBr: T, elseBr: T): T = macro ifThenElseImpl[T]
//   def __return(expr: Any): Nothing = macro returnImpl
//   def __assign[T](lhs: T, rhs: T): Unit = macro assignImpl[T]
//   def __whileDo(cond: Boolean, body: Unit): Unit = macro whileDoImpl
//   def __doWhile(body: Unit, cond: Boolean): Unit = macro doWhileImpl
//   def __newVar[T](init: T): T = macro newVarImpl[T]
// }

// /**
//  * EmbeddedControls companion object containing macro implementations.
//  *
//  * This companion object servers a dual purpose: it contains the macro
//  * implementations of the [[EmbeddedControl]] macros, and it enables
//  */
// private object EmbeddedControls {

//   def ifThenElseImpl[T: c.WeakTypeTag](c: Context)(
//     cond: c.Expr[Boolean], thenBr: c.Expr[T], elseBr: c.Expr[T]): c.Expr[T] = {

//     import c.universe._
//     c.Expr[T](q"if ($cond) $thenBr else $elseBr")
//   }

//   def returnImpl(c: Context)(expr: c.Expr[Any]): c.Expr[Nothing] = {

//     import c.universe._
//     c.Expr[Nothing](q"return $expr")
//   }

//   def assignImpl[T: c.WeakTypeTag](c: Context)(
//     lhs: c.Expr[T], rhs: c.Expr[T]): c.Expr[Unit] = {

//     import c.universe._
//     c.Expr[Unit](q"$lhs = $rhs")
//   }

//   def whileDoImpl(c: Context)(
//     cond: c.Expr[Boolean], body: c.Expr[Unit]): c.Expr[Unit] = {

//     import c.universe._
//     c.Expr[Unit](q"while ($cond) $body")
//   }

//   def doWhileImpl(c: Context)(
//     body: c.Expr[Unit], cond: c.Expr[Boolean]): c.Expr[Unit] = {

//     import c.universe._
//     c.Expr[Unit](q"do $body while ($cond)")
//   }

//   def newVarImpl[T: c.WeakTypeTag](c: Context)(
//     init: c.Expr[T]): c.Expr[T] = init
// }

