package lifted

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

/*
 * This is a prototype implementation of the embedded DSL. In this prototype we will use the of
 * polymorphic embedding of DSLs.
 *
 * DSL. Once the library is complete all method implementations should
 * remain empty so users can build their own embedded compilers. There is need to enforce more than lifting on the library user.
 */
trait LiftBase {

  trait LiftEvidence[T, U] {
    def lift(v: T): U
  }

  def liftTerm[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv.lift(v)
}

trait ScalaDSL extends ScalaOpsPkg with ScalaOpsPkgExp with LiftBase { self ⇒

  implicit def liftAny[T: Manifest]: LiftEvidence[T, Rep[T]] =
    new LiftEvidence[T, Rep[T]] {
      def lift(v: T): Rep[T] = unit(v)
    }

  def main(): Any

  def interpret[T: Manifest](): T = {
    val codegen = new ScalaCodeGenPkg {
      val IR: self.type = self
    }

    // main1().asInstanceOf[Exp[_]]
    codegen.emitSource0(main.asInstanceOf[Exp[T]], "Test", new PrintWriter(System.out))
    2.asInstanceOf[T]
  }

  // I need an Exp and a codegen
}
