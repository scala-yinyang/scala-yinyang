package failure

import scala.annotation.unchecked.uncheckedVariance

trait FailureCake {
  //  type Rep[T] = Exp[T]
  //
  //  abstract class Exp[+T: Manifest] {
  //    def tp: Manifest[T @uncheckedVariance] = manifest[T]
  //  }

  //  trait LiftEvidence[T, K]
  implicit def liftAnyFails[T: Manifest]: Any = ???
  // This one works
  //  implicit def liftAny[T]: Any = ???
  //    new LiftEvidence[T, Rep[T]] {
  //      def lift(v: T): Rep[T] = ???
  //    }

}