package ch.epfl.yinyang
package analysis

import ch.epfl.yinyang._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable
import mutable.{ ListBuffer, HashMap }

/**
 * Analysed free variables in the block. Free vars are the idents that are not
 * defined in the block.
 *
 * TODO either here or at hole transformer support the fields and methods without params.
 *
 */
trait FreeIdentAnalysis extends MacroModule with TransformationUtils with YYConfig {
  import c.universe._

  // Symbol tracking.
  val symbolIds: mutable.HashMap[Int, Symbol] = new mutable.HashMap()
  def symbolById(id: Int) = symbolIds(id)

  def freeVariables(tree: Tree): List[Symbol] =
    new FreeVariableCollector().collect(tree)

  class FreeVariableCollector extends Traverser {

    private[this] val collected = ListBuffer[Symbol]()
    private[this] var defined = List[Symbol]()

    private[this] final def isFree(id: Symbol) = !defined.contains(id)

    override def traverse(tree: Tree) = tree match {
      case i @ Ident(s) => {
        val sym = i.symbol
        //store info about idents
        symbolIds.put(symbolId(sym), sym)
        if (sym.isTerm &&
          !(sym.isMethod || sym.isPackage || sym.isModule) &&
          isFree(sym)) collected append sym
      }
      case _ => super.traverse(tree)
    }

    def collect(tree: Tree): List[Symbol] = {
      collected.clear()
      defined = new LocalDefCollector().definedSymbols(tree)
      log(s"Defined: $defined")
      traverse(tree)
      collected.toList.distinct
    }

  }

  class LocalDefCollector extends Traverser {

    private[this] val definedValues, definedMethods = ListBuffer[Symbol]()

    override def traverse(tree: Tree) = tree match {
      case vd @ ValDef(mods, name, tpt, rhs) =>
        definedValues += vd.symbol
        traverse(rhs)
      case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        definedMethods += dd.symbol
        vparamss.flatten.foreach(traverse)
        traverse(rhs)
      case _ =>
        super.traverse(tree)
    }

    def definedSymbols(tree: Tree): List[Symbol] = {
      definedValues.clear()
      definedMethods.clear()
      traverse(tree)
      (definedValues ++ definedMethods).toList
    }

  }
}