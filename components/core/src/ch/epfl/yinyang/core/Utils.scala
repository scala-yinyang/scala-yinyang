package ch.epfl.yinyang

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros

trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DataDefs extends MacroModule {
  import c.universe._
  case class DSLFeature(tpe: Option[Type], name: String, targs: List[Tree], args: List[List[Type]])
}

/**
 * Common utilities for the Yin-Yang project.
 */
trait TransformationUtils extends MacroModule {
  import c.universe._

  /* These two should be unified */
  def method(recOpt: Option[Tree], methName: String, args: List[List[Tree]], targs: List[Tree] = Nil): Tree = {
    val calleeName = newTermName(methName)
    val callee = recOpt match {
      case Some(rec) => Select(rec, calleeName)
      case None      => Ident(calleeName)
    }
    val calleeAndTargs: Tree = typeApply(targs)(callee)
    args.foldLeft(calleeAndTargs) { Apply(_, _) }
  }

  private[yinyang] def symbolId(symbol: Symbol): Int =
    symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id

  private[yinyang] def symbolId(tree: Tree): Int = symbolId(tree.symbol)

  def typeApply(targs: List[Tree])(select: Tree) = if (targs.nonEmpty)
    TypeApply(select, targs)
  else
    select

  def makeConstructor(classname: String, arguments: List[Tree]): Tree =
    Apply(Select(newClass(classname), nme.CONSTRUCTOR), arguments)

  def newClass(classname: String) =
    New(Ident(newTypeName(classname)))

  def copy(orig: Tree)(nev: Tree): Tree = {
    nev.setSymbol(orig.symbol)
    nev.setPos(orig.pos)
    nev
  }

  def log(s: => String, level: Int = 0) = if (debugLevel > level) println(s)

  def debugLevel: Int
}
