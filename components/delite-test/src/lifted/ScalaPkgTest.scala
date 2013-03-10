package lifted

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.ScalaCompile
import scala.virtualization.lms.util.OverloadHack
import ch.epfl.lamp.mpde.api._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.io._

/*
 * This is a prototype implementation of the embedded DSL. In this prototype we will use the of
 * polymorphic embedding of DSLs.
 *
 * DSL. Once the library is complete all method implementations should
 * remain empty so users can build their own embedded compilers. There is need to enforce more than lifting on the library user.
 */
trait LiftBase {

  abstract class LiftEvidence[T: Manifest, U] {
    def lift(v: T): U
  }

  abstract class HoleEvidence[T: Manifest, U] {
    def hole(v: Long): U
  }

  def liftTerm[T, Ret](v: T)(implicit liftEv: LiftEvidence[T, Ret]): Ret =
    liftEv.lift(v)

  //  private def hole[T: Manifest, Ret](symId: Long)(holeEv: HoleEvidence[T, Ret]): Ret =
  //    holeEv.hole(symId)
}

trait ScalaDSL extends ScalaOpsPkg with ScalaOpsPkgExp with LiftBase with Base
  with ScalaCompile { self ⇒

  case class Hole[+T: Manifest](symId: Long) extends Def[T]

  implicit def liftAny[T: Manifest]: LiftEvidence[T, Rep[T]] =
    new LiftEvidence[T, Rep[T]] {
      def lift(v: T): Rep[T] = unit(v)
    }

  def hole[T: Manifest](symId: Long): Rep[T] = toAtom(Hole(symId))
  //  implicit def holeAny[T: Manifest]: HoleEvidence[T, Rep[T]] =
  //    new HoleEvidence[T, Rep[T]] {
  //      def hole(v: Long): Rep[T] = toAtom(Hole(v))
  //    }

  def main(): Any

  val codegen = new ScalaCodeGenPkg {
    val IR: self.type = self

    override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
      case Hole(x) ⇒
      case _       ⇒ super.emitNode(sym, rhs)
    }

    def emitSourceYinYang[T: Manifest](f: Exp[T], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
      val body = reifyBlock(f)

      val syms: List[Sym[_]] = focusBlock(body) {
        innerScope flatMap {
          case TP(sym, rhs) ⇒
            rhs match {
              case Hole(x) ⇒ scala.List(sym)
              case _       ⇒ Nil
            }
          case _ ⇒ Nil
        }
      }
      emitSource(syms, body, className, stream)
    }

  }

  /*
   * Ret must be Nothing* => T. If I was only smarter to make this work without a convention :/
   */
  def compile[T: Manifest, Ret] = {

    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1
    val source = new StringWriter()
    codegen.emitSourceYinYang(main.asInstanceOf[Exp[T]], className, new PrintWriter(source))

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    Predef.println(source)
    run.compileSources(scala.List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    reporter.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    cls.getConstructor().newInstance().asInstanceOf[Ret]
  }

  def interpret[T: Manifest](params: Nothing*): T = {
    params.length match {
      case 0 ⇒
        compile[T, () ⇒ T].apply
      case 1 ⇒
        compile[T, Any ⇒ T].apply(params(0))
      case 2 ⇒
        compile[T, (Any, Any) ⇒ T].apply(params(0), params(1))
    }

  }

}
