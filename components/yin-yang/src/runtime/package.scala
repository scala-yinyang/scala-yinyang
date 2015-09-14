package ch.epfl.yinyang

/*
 * Pattern matcher for the direct embedding.
 * Note: Should be imported only by the framework--never manually.
 */
package object runtime {
  object __match {
    def zero: Option[Nothing] = None
    def one[T](x: T): Option[T] = Some(x)
    def guard[T](cond: Boolean, then: => T): Option[T] =
      if (cond) one(then) else zero
    def runOrElse[T, U](x: T)(f: T => Option[U]): U =
      f(x) getOrElse (throw new MatchError(x))
  }
}
