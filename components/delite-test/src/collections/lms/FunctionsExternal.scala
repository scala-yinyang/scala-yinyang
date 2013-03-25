package collections.lms

import scala.collection.mutable

import scala.reflect.SourceContext

import scala.virtualization.lms.common.{ BlockExp, FunctionsExp, ScalaGenFunctions }
import scala.virtualization.lms.util.ClosureCompare

// Why FunctionsExternalDef instead of FunctionsExternalExp?
trait FunctionsExternalDef extends FunctionsExp with BlockExp with ClosureCompare {
  case class DefineFun[A, B](res: Block[B])(val arg: Sym[A]) extends Def[A ⇒ B]

  override def syms(e: Any): List[Sym[Any]] = e match {
    case DefineFun(y) ⇒ syms(y)
    case _            ⇒ super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case f @ DefineFun(y) ⇒ f.arg :: effectSyms(y)
    case _                ⇒ super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case DefineFun(y) ⇒ freqHot(y)
    case _            ⇒ super.symsFreq(e)
  }

  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A: Manifest, B: Manifest](f: Exp[A] ⇒ Exp[B])(implicit pos: SourceContext): Exp[A ⇒ B] = {
    var can = canonicalize(f)

    funTable.find(_._2 == can) match {
      case Some((funSym, _)) ⇒
        funSym.asInstanceOf[Sym[A ⇒ B]]
      case _ ⇒
        val funSym = fresh[A ⇒ B]
        val argSym = fresh[A]

        funTable = (funSym, can) :: funTable

        val y = reifyEffects(f(argSym))

        createDefinition(funSym, DefineFun[A, B](y)(argSym))
        funSym
    }
  }
}

trait ScalaGenFunctionsExternal extends ScalaGenFunctions {
  val IR: FunctionsExternalDef
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e @ DefineFun(y) ⇒
      emitValDef(sym, "{" + quote(e.arg) + ": (" + e.arg.tp + ") => " /*}*/ )
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case _ ⇒ super.emitNode(sym, rhs)
  }
}
