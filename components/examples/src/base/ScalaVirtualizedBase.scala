import ch.epfl.yinyang.example._
package ch.epfl.yinyang.polymorphic {

  trait VirtualControlsBase extends PolymorphicBase {
    def $ifThenElse[T](cnd: R[Boolean], thn: => R[T], els: => R[T]): R[T]
    def $return(expr: R[Any]): R[Nothing]
    def $whileDo(cnd: R[Boolean], body: => R[Unit]): R[Unit]
    def $doWhile(body: => R[Unit], cond: R[Boolean]): R[Unit]
    def $try[T](body: => R[T], b: R[Throwable => T], fin: => R[T]): R[T]
    def $throw(e: R[Throwable]): R[Nothing]
  }

  trait VirtualVariablesBase extends PolymorphicBase {
    def $valDef[T](init: R[T]): R[T]
    def $lazyValDef[T](init: => R[T]): R[T]
    def $varDef[T](init: R[T]): R[T]
    def $read[T](init: R[T]): R[T]
    def $assign[T](lhs: R[T], rhs: R[T]): R[Unit]
  }

  trait VirtualAnyBase extends PolymorphicBase {
    def $infix_==(x1: R[Any], x2: R[Any]): R[Boolean]
    def $infix_!=(x1: R[Any], x2: R[Any]): R[Boolean]
    def $infix_##(x: R[Any]): R[Int]
    def $infix_hashCode(x: R[Any]): R[Int]
    def $infix_asInstanceOf[T](x: R[Any]): R[T]
    def $infix_isInstanceOf[T](x: R[Any]): R[Boolean]
    def $infix_getClass(x: R[Any]): R[Class[_]]
    def $infix_toString(x: R[Any]): R[String]
  }

  trait VirtualAnyRefBase extends PolymorphicBase {
    def $infix_eq(x1: R[AnyRef], x2: R[AnyRef]): R[Boolean]
    def $infix_ne(x1: R[AnyRef], x2: R[AnyRef]): R[Boolean]
    def $infix_notify(x: R[AnyRef]): R[Unit]
    def $infix_notifyAll(x: R[AnyRef]): R[Unit]
    def $infix_synchronized[T](x: R[AnyRef], body: R[T]): R[T]
    def $infix_wait(x: R[AnyRef]): R[Unit]
    def $infix_wait(x: R[AnyRef], timeout: R[Long]): R[Unit]
    def $infix_wait(x: R[AnyRef], timeout: R[Long], nanos: R[Int]): R[Unit]
  }

  package generic {
    trait VirtualFunctionsBase extends PolymorphicBase {
      def $app[U](f: R[() => U]): () => R[U]
      def $app[T_1, U](f: R[T_1 => U]): R[T_1] => R[U]
      def $app[T_1, T_2, U](f: R[(T_1, T_2) => U]): (R[T_1], R[T_2]) => R[U]

      def $lam[U](f: () => R[U]): R[() => U]
      def $lam[T_1, U](f: R[T_1] => R[U]): R[T_1 => U]
      def $lam[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): R[(T_1, T_2) => U]
    }
  }

  // This is used only for the translation where R[X] => R[Y] => R[Z] is forbidden.
  // For the curried version function virtualization must be disabled.
  package inlining {
    trait VirtualFunctionsBase extends PolymorphicBase {
      def $app[U](f: R[() => U]): () => R[U]
      def $app[T_1, U](f: R[T_1] => R[U]): R[T_1] => R[U]
      def $app[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): (R[T_1], R[T_2]) => R[U]

      def $lam[U](f: () => R[U]): () => R[U]
      def $lam[T_1, U](f: R[T_1] => R[U]): R[T_1] => R[U]
      def $lam[T_1, T_2, U](f: (R[T_1], R[T_2]) => R[U]): (R[T_1], R[T_2]) => R[U]
    }
  }
}

package identity {
  trait IdentityBase {
    def $ifThenElse[T](cond: Boolean, thenBr: => T, elseBr: => T): T
    def $return(expr: Any): Nothing
    def $whileDo(cond: Boolean, body: => Unit): Unit
    def $doWhile(body: => Unit, cond: Boolean): Unit
    def $try[T](body: => T, catches: Throwable => T, finalizer: => T): T
    def $throw(t: Throwable): Nothing

    def $valDef[T](init: T): T
    def $lazyValDef[T](init: => T): T
    def $varDef[T](init: T): T
    def $read[T](init: T): T
    def $assign[T](lhs: T, rhs: T): Unit
  }

  trait IdentityFunctionsBase {
    def $app[U](f: () => U): () => U
    def $app[T_1, U](f: T_1 => U): T_1 => U
    def $app[T_1, T_2, U](f: (T_1, T_2) => U): (T_1, T_2) => U

    def $lam[U](f: () => U): () => U
    def $lam[T_1, U](f: T_1 => U): T_1 => U
    def $lam[T_1, T_2, U](f: (T_1, T_2) => U): (T_1, T_2) => U
  }

  trait VirtualAnyBase {
    def $infix_==(x1: Any, x2: Any): Boolean
    def $infix_!=(x1: Any, x2: Any): Boolean
    def $infix_##(x: Any): Int
    def $infix_hashCode(x: Any): Int
    def $infix_asInstanceOf[T](x: Any): T
    def $infix_isInstanceOf[T](x: Any): Boolean
    def $infix_getClass(x: Any): Class[_]
    def $infix_toString(x: Any): String
  }

  trait VirtualAnyRefBase {
    def $infix_eq(x1: AnyRef, x2: AnyRef): Boolean
    def $infix_ne(x1: AnyRef, x2: AnyRef): Boolean
    def $infix_notify(x: AnyRef): Unit
    def $infix_notifyAll(x: AnyRef): Unit
    def $infix_synchronized[T](x: AnyRef, body: T): T
    def $infix_wait(x: AnyRef): Unit
    def $infix_wait(x: AnyRef, timeout: Long): Unit
    def $infix_wait(x: AnyRef, timeout: Long, nanos: Int): Unit
  }
}
package custom {
  trait AbstractTypesBase {
    type Boolean
    type Any
    type Nothing
    type Unit
    type Throwable
    def $ifThenElse[T](cond: Boolean, thenBr: => T, elseBr: => T): T
    def $return(expr: Any): Nothing
    def $whileDo(cond: Boolean, body: => Unit): Unit
    def $doWhile(body: => Unit, cond: Boolean): Unit
    def $try[T](body: => T, catches: Throwable => T, finalizer: => T): T
    def $throw(t: Throwable): Nothing

    def $valDef[T](init: T): T
    def $lazyValDef[T](init: T): T
    def $varDef[T](init: T): T
    def $read[T](init: T): T
    def $assign[T](lhs: T, rhs: T): Unit
  }

  trait VirtualFunctionsBase {
    type Function0[U]
    type Function1[T_1, U]
    type Function2[T_1, T_2, U]
    def $app[U](f: Function0[U]): () => U
    def $app[T_1, U](f: Function1[T_1, U]): T_1 => U
    def $app[T_1, T_2, U](f: Function2[T_1, T_2, U]): (T_1, T_2) => U

    def $lam[U](f: () => U): Function0[U]
    def $lam[T_1, U](f: T_1 => U): Function1[T_1, U]
    def $lam[T_1, T_2, U](f: (T_1, T_2) => U): Function2[T_1, T_2, U]
  }

  trait VirtualAnyBase {
    type Any
    type Boolean
    type Int
    type Class[T]
    type String
    def $infix_==(x1: Any, x2: Any): Boolean
    def $infix_!=(x1: Any, x2: Any): Boolean
    def $infix_##(x: Any): Int
    def $infix_hashCode(x: Any): Int
    def $infix_asInstanceOf[T](x: Any): T
    def $infix_isInstanceOf[T](x: Any): Boolean
    def $infix_getClass(x: Any): Class[_]
    def $infix_toString(x: Any): String
  }

  trait VirtualAnyRefBase {
    type AnyRef
    type Boolean
    type Int
    type Long
    type Unit
    def $infix_eq(x1: AnyRef, x2: AnyRef): Boolean
    def $infix_ne(x1: AnyRef, x2: AnyRef): Boolean
    def $infix_notify(x: AnyRef): Unit
    def $infix_notifyAll(x: AnyRef): Unit
    def $infix_synchronized[T](x: AnyRef, body: T): T
    def $infix_wait(x: AnyRef): Unit
    def $infix_wait(x: AnyRef, timeout: Long): Unit
    def $infix_wait(x: AnyRef, timeout: Long, nanos: Int): Unit
  }
}
