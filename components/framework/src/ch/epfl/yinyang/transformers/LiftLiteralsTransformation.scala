package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Ascribes terms with their types from the original block. Terms that are ascribed are:
 *   - applications
 *   - idents
 *   - lambda parameters
 */
trait LiftLiteralTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(idents: List[Symbol] = Nil)(tree: Tree) =
      new LiftLiteralTransformer(idents).transform(tree)
  }

  class LiftLiteralTransformer(val idents: List[Symbol])
    extends Transformer {

    def lift(t: Tree) = {
      Apply(Select(This(tpnme.EMPTY), newTermName("lift")), List(t))
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) =>
          lift(t)
        case t @ Ident(_) if idents.contains(t.symbol) =>
          lift(t)
        case _ =>
          super.transform(tree)
      }
    }
  }

}