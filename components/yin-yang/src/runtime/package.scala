package ch.epfl.yinyang

import reflect.runtime.universe._

/*
 * Pattern matcher for the direct embedding.
 * Note: Should be imported only by the framework--never manually.
 */
package object shallow {
  object __match {
    def zero: Option[Nothing] = None
    def one[T](x: T): Option[T] = Some(x)
    def guard[T](cond: Boolean, thn: => T): Option[T] =
      if (cond) one(thn) else zero
    def runOrElse[T, U](x: T)(f: T => Option[U]): U =
      f(x) getOrElse (throw new MatchError(x))
  }

  // Used for type-checking the intermediate steps of the translation.
  // Note: methods do not have an implementation since they should never be executed.

  def lift[T](v: T): T = ???
  def hole[T](ttag: TypeTag[T], id: Int): T = ???

  def $ifThenElse[T](cond: Boolean, thenBr: => T, elseBr: => T): T = ???
  def $return(expr: Any): Nothing = ???
  def $whileDo(cond: Boolean, body: => Unit): Unit = ???
  def $doWhile(body: => Unit, cond: Boolean): Unit = ???
  def $try[T](body: => T, catches: Throwable => T, finalizer: => T): T = ???
  def $throw(t: Throwable): Nothing = ???

  def $valDef[T](init: T): T = ???
  def $lazyValDef[T](init: => T): T = ???
  def $varDef[T](init: T): T = ???
  def $read[T](init: T): T = ???
  def $assign[T](lhs: T, rhs: T): Unit = ???

  def $infix_==(x1: Any, x2: Any): Boolean = ???
  def $infix_!=(x1: Any, x2: Any): Boolean = ???
  def $infix_##(x: Any): Int = ???
  def $infix_hashCode(x: Any): Int = ???
  def $infix_asInstanceOf[T](x: Any): T = ???
  def $infix_isInstanceOf[T](x: Any): Boolean = ???
  def $infix_getClass(x: Any): Class[_] = ???

  def $infix_eq(x1: AnyRef, x2: AnyRef): Boolean = ???
  def $infix_ne(x1: AnyRef, x2: AnyRef): Boolean = ???
  def $infix_notify(x: AnyRef): Unit = ???
  def $infix_notifyAll(x: AnyRef): Unit = ???
  def $infix_synchronized[T](x: AnyRef, body: T): T = ???
  def $infix_wait(x: AnyRef): Unit = ???
  def $infix_wait(x: AnyRef, timeout: Long): Unit = ???
  def $infix_wait(x: AnyRef, timeout: Long, nanos: Int): Unit = ???

  def $app[U](f: () => U): () => U = ???
  def $lam[U](f: () => U): () => U = ???
  def $app[T_1, U](f: (T_1) => U): (T_1) => U = ???
  def $lam[T_1, U](f: (T_1) => U): (T_1) => U = ???
  def $app[T_1, T_2, U](f: (T_1, T_2) => U): (T_1, T_2) => U = ???
  def $lam[T_1, T_2, U](f: (T_1, T_2) => U): (T_1, T_2) => U = ???
  def $app[T_1, T_2, T_3, U](f: (T_1, T_2, T_3) => U): (T_1, T_2, T_3) => U = ???
  def $lam[T_1, T_2, T_3, U](f: (T_1, T_2, T_3) => U): (T_1, T_2, T_3) => U = ???
  def $app[T_1, T_2, T_3, T_4, U](f: (T_1, T_2, T_3, T_4) => U): (T_1, T_2, T_3, T_4) => U = ???
  def $lam[T_1, T_2, T_3, T_4, U](f: (T_1, T_2, T_3, T_4) => U): (T_1, T_2, T_3, T_4) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, U](f: (T_1, T_2, T_3, T_4, T_5) => U): (T_1, T_2, T_3, T_4, T_5) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, U](f: (T_1, T_2, T_3, T_4, T_5) => U): (T_1, T_2, T_3, T_4, T_5) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, U](f: (T_1, T_2, T_3, T_4, T_5, T_6) => U): (T_1, T_2, T_3, T_4, T_5, T_6) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, U](f: (T_1, T_2, T_3, T_4, T_5, T_6) => U): (T_1, T_2, T_3, T_4, T_5, T_6) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21) => U = ???
  def $app[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22) => U = ???
  def $lam[T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22, U](f: (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22) => U): (T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, T_19, T_20, T_21, T_22) => U = ???

}
