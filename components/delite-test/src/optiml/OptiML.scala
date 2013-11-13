package ch.epfl.lamp.optiml

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.internal.ScalaCompile
import scala.virtualization.lms.util.OverloadHack
import ch.epfl.yinyang.api._
import scala.reflect.runtime.universe._
import reflect.ClassTag
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.SourceContext
import java.io._
import lms.{ ScalaDSL, LMSYinYang, YinYangLMSCompile }
import autooptiml.compiler.AutoOptiMLApplicationCompiler

trait OptiMLDSL extends AutoOptiMLApplicationCompiler with LMSDelite with YinYangDeliteInterpret { self =>
  type Int = scala.Int
  type Long = scala.Long
  type Double = scala.Double
  type Float = scala.Float
  type Char = scala.Char
  type Boolean = scala.Boolean
  type Unit = scala.Unit
  type String = scala.Predef.String
  type Seq[T] = scala.Seq[T]
  type Tuple3[T1, T2, T3] = scala.Tuple3[T1, T2, T3]
  // type Tuple6[T1, T2, T3, T4, T5, T6] = scala.Tuple6[T1, T2, T3, T4, T5, T6]
  type Tuple6[T1, T2, T3, T4, T5, T6] = Tup6[T1, T2, T3, T4, T5, T6]
  implicit class RepSeq[T](v: Rep[Seq[T]]) {
    def apply(index: Rep[Int]) = ???
  }
  object Seq {
    def apply[T](args: Rep[T]*): Rep[Seq[T]] = ???
  }
  // type Array[T] = scala.Array[T]

  // val Array = scala.Array

  def IntOps(v: Rep[Int]): Rep[Int] = v

  def VarSeq[T](v: Rep[T]*): Seq[Rep[T]] = v.toSeq

  object Tuple2 {
    def apply[T1, T2](_1: Rep[T1], _2: Rep[T2]): Tuple2[Rep[T1], Rep[T2]] = (_1, _2)
    // def apply[T1, T2](_1: Rep[T1], _2: Rep[T2]): Rep[Tuple2[T1, T2]] = ???
  }

  object Tuple3 {
    // def apply[T1, T2, T3](_1: Rep[T1], _2: Rep[T2], _3: Rep[T3]): Tuple3[Rep[T1], Rep[T2], Rep[T3]] = (_1, _2, _3)
    // FIXME wh
    def apply[T1, T2, T3](_1: Rep[T1], _2: Rep[T2], _3: Rep[T3]): Rep[Tuple3[T1, T2, T3]] = ???
    // def unapply[T1, T2, T3](x: Rep[Tuple3[T1, T2, T3]]): Option[Tuple3[Rep[T1], Rep[T2], Rep[T3]]] = ???
  }

  object Tuple6 {
    // def apply[T1, T2, T3](_1: Rep[T1], _2: Rep[T2], _3: Rep[T3]): Tuple3[Rep[T1], Rep[T2], Rep[T3]] = (_1, _2, _3)
    // FIXME wh
    def apply[T1, T2, T3, T4, T5, T6](_1: Rep[T1], _2: Rep[T2], _3: Rep[T3], _4: Rep[T4], _5: Rep[T5], _6: Rep[T6]): Rep[Tuple6[T1, T2, T3, T4, T5, T6]] = ???
    // def unapply[T1, T2, T3](x: Rep[Tuple3[T1, T2, T3]]): Option[Tuple3[Rep[T1], Rep[T2], Rep[T3]]] = ???
  }

  def infix_!=[A: Manifest, B: Manifest](a: Rep[A], b: Rep[B])(implicit sc: SourceContext): Rep[Boolean] = equals(a, b)

  implicit def tup3ToRep[T1, T2, T3](x: Rep[Tuple3[T1, T2, T3]]): Tuple3[Rep[T1], Rep[T2], Rep[T3]] = ???

  class AString(v: Rep[String]) {
    // def toDouble(implicit pos: SourceContext): Rep[Double] = infix_toDouble(v)(pos, Overload3)
    def toDouble: Rep[Double] = ???
    def toInt: Rep[Int] = ???
  }

  implicit def liftIndexWildcard(v: *.type): IndexWildcard with Rep[*.type] = ???
  // implicit def unliftIndexWildcard(v: Rep[*.type]): IndexWildcard = *
  implicit def liftToIndexVectorTuple2IndexVectorIndexWildcardOpsClsYY(x: Tuple2[Rep[IndexVector], Rep[IndexWildcard]]): IndexVectorTuple2IndexVectorIndexWildcardOpsCls = ???

  implicit def tupleToDense2yy[T: Manifest](t: Tuple2[Rep[T], Rep[T]]): Rep[DenseVector[T]] = DenseVector[T]((t._1), (t._2))
  def augmentString(v: Rep[String]): AString = new AString(v)
  def toForgeExtras[T](x: Rep[T]): Rep[T] = x

  override def main(): Unit = ???
}

trait LMSDelite extends BaseYinYangManifest with FullyStaged with BaseExp with Expressions with Effects { self =>
  case class Hole[+T: Manifest](symId: Long) extends Def[T]

  implicit def liftAny[T: Manifest]: LiftEvidence[T, Rep[T]] =
    new LiftEvidence[T, Rep[T]] {
      def lift(v: T): Rep[T] = unit(v)
      def hole(m: TypeRep[T], symId: Int): Rep[T] = toAtom(Hole(symId))
    }

  def mainDelite(): Any

}

trait YinYangDeliteInterpret extends Interpreted { this: LMSDelite =>

  /**
   * Accepts the captured values and returns the result.
   */
  def interpret[T: TypeRep](params: Any*): T = {
    mainDelite().asInstanceOf[T]
  }
}

trait YinYangDeliteCompile extends CodeGenerator with ScalaCompile { self: LMSDelite =>
  val codegenDelite: YinYangDeliteGenerator { val IR: YinYangDeliteCompile.this.type }

  def generateCode(className: String) = {
    val source = new StringWriter()
    codegenDelite.emitSourceYinYang(mainDelite.asInstanceOf[codegenDelite.IR.Exp[Any]], className, new PrintWriter(source))
    source.toString
  }

  /*
   * Ret must be Any* => T. If I was only smarter to make this work without a convention :/
   */
  def compile[T: Manifest, Ret] = {

    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1
    val source = new StringWriter()
    codegenDelite.emitSourceYinYang(mainDelite.asInstanceOf[codegenDelite.IR.Exp[T]], className, new PrintWriter(source))

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    run.compileSources(scala.List(new BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    reporter.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    cls.getConstructor().newInstance().asInstanceOf[Ret]
  }

  def interpret[T: Manifest](params: Any*): T = {
    params.length match {
      case 0 =>
        compile[T, () => T].apply
      case 1 =>
        compile[T, Any => T].apply(params(0))
      case 2 =>
        compile[T, (Any, Any) => T].apply(params(0), params(1))
    }
  }
}

trait YinYangDeliteGenerator extends ScalaNestedCodegen with ScalaCodegen {
  val IR: LMSDelite
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Hole(x) =>
    case _       => super.emitNode(sym, rhs)
  }

  def emitSourceYinYang(f: Exp[Any], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val body = reifyBlock(f)(f.tp)

    val syms: List[Sym[_]] = focusBlock(body) {
      innerScope flatMap {
        case TP(sym, rhs) =>
          rhs match {
            case Hole(x) => scala.List(sym)
            case _       => Nil
          }
        case _ => Nil
      }
    }
    emitSource(syms, body, className, stream)(body.tp)
  }
}