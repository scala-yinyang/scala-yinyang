package ch.epfl.yinyang
package transformers

import language.experimental.macros

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.Universe
import scala.collection.mutable

/**
 * Pre-processing phase, which allows pre-processing transformations.
 */
class PreProcessing[C <: Context](val c: C)(val patterns: List[PartialFunction[Universe#Tree, Universe#Tree]]) {
  import c.universe._

  val PreProcess = new (Tree => Tree) {
    def apply(tree: Tree) = new PreProcess().transform(tree)
  }

  private final class PreProcess extends Transformer {
    override def transform(tree: Tree): Tree = {
      val res = patterns collectFirst {
        case pat if pat.isDefinedAt(tree) => pat(tree)
      } getOrElse super.transform(tree)
      res.asInstanceOf[Tree]
    }
  }
}

class NullPreProcessing[C <: Context](ctx: C) extends PreProcessing(ctx)(Nil) {
  import c.universe._
  override object PreProcess extends (Tree => Tree) {
    def apply(tree: Tree) = tree
  }
}
