package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Lifts constants by wrapping them in a lift(<lit>) and renames and lifts the
 * in toLift provided captured identifiers from outside the DSL scope by
 * wrapping them in lift(captured$<id>). The toLiftHole identifiers will be
 * wrapped in lift(captured$<id>, Ident(t)) and the second argument will be
 * transformed into a hole() call by the HoleTransformer.
 */
trait LiftLiteralTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(toLift: List[Symbol], toLiftHole: List[Symbol])(tree: Tree) = {
      val t = new LiftLiteralTransformer(toLift, toLiftHole).transform(tree)
      log("lifted: " + t, 2)
      t
    }
  }

  class LiftLiteralTransformer(toLift: List[Symbol], toLiftHole: List[Symbol])
    extends Transformer {

    def lift(t: List[Tree]) = {
      Apply(Select(This(tpnme.EMPTY), newTermName("lift")), t)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) =>
          lift(List(t))
        case t @ Ident(_) if toLift.contains(t.symbol) =>
          lift(List(Ident(newTermName("captured$" + t.name.decoded))))
        case t @ Ident(_) if toLiftHole.contains(t.symbol) =>
          lift(List(Ident(newTermName("captured$" + t.name.decoded)), Apply(Ident(newTermName("Some")), List(t))))
        case _ =>
          super.transform(tree)
      }
    }
  }

}