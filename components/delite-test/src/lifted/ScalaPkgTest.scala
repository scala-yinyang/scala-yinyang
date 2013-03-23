package lifted

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.ScalaCompile
import scala.virtualization.lms.util.OverloadHack
import ch.epfl.lamp.yinyang.api._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.io._

trait LMSYinYang extends BaseYinYang with BaseExp { self ⇒
  case class Hole[+T: Manifest](symId: Long) extends Def[T]

  implicit def liftAny[T: Manifest]: LiftEvidence[T, Rep[T]] =
    new LiftEvidence[T, Rep[T]] {
      def lift(v: T): Rep[T] = unit(v)
      def hole(m: Manifest[Any], symId: Int): Rep[T] = toAtom(Hole(symId))
    }

  def stagingAnalyze() = Nil
}

trait ScalaDSL extends ScalaOpsPkg with ScalaOpsPkgExp with LMSYinYang with CodeGenerator
  with ScalaCompile { self ⇒

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

import ppl.dsl.optiml._

trait OptiML extends OptiMLApplicationRunner with LMSYinYang with Interpreted {
  def mainDelite(): Any

  override def main(): Unit = ???

  def interpret[T: Manifest](params: Any*) = 0.asInstanceOf[T]

  type Boolean = scala.Boolean
  type Int = scala.Int
  type Unit = scala.Unit
  type Nothing = scala.Nothing
  type Any = scala.Any
}

import ppl.dsl.optigraph._

trait OptiGraph extends OptiGraphApplicationRunner with LMSYinYang with Interpreted {
  def mainDelite(): Any

  override def main(): Unit = ???

  def interpret[T: Manifest](params: Any*) = 0.asInstanceOf[T]

  type Int = scala.Int
  type Float = scala.Float
  type Double = scala.Double
  type Boolean = scala.Boolean
  type String = Predef.String
  type Unit = scala.Unit
  type Nothing = scala.Nothing
  type Any = scala.Any
  type Array[T] = scala.Array[T]

  type Graph = ppl.dsl.optigraph.Graph
  type Node = ppl.dsl.optigraph.Node
  type Edge = ppl.dsl.optigraph.Edge
  type GSet[T] = ppl.dsl.optigraph.GSet[T]
  type GSeq[T] = ppl.dsl.optigraph.GSeq[T]
  type GOrder[T] = ppl.dsl.optigraph.GOrder[T]
  type GIterable[T] = ppl.dsl.optigraph.GIterable[T]
  type Deferrable[T] = ppl.dsl.optigraph.Deferrable[T]
  type Reduceable[T] = ppl.dsl.optigraph.Reduceable[T]
  type NodeProperty[T] = ppl.dsl.optigraph.NodeProperty[T]
  type EdgeProperty[T] = ppl.dsl.optigraph.EdgeProperty[T]

  override implicit def repNodeToNodeOps(n: Rep[Node]) = new NodeOpsCls(n)

  //  implicit object LanguageObj {
  //    def MIN_INT = 128
  //  }

  //implicit object IntIsIntegral extends Numeric.IntIsIntegral with Ordering.IntOrdering

  implicit val ManifestFactory = scala.reflect.ManifestFactory
  implicit val IntIsIntegral = scala.math.Numeric.IntIsIntegral
  val DoubleIsFractional = scala.math.Numeric.DoubleIsFractional

  //  def ====(ths: Rep[Any], t: Rep[Any]): Rep[Boolean] = ths == t

}
