package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Lifts constants by wrapping them in a lift(<lit>) and renames and lifts the
 * provided captured identifiers from outside the DSL scope by wrapping them in
 * lift(captured$<id>).
 */
trait LiftLiteralTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(idents: List[Symbol] = Nil)(tree: Tree) = {
      val t = new LiftLiteralTransformer(idents).transform(tree)
      log("lifted: " + t, 2)
      t
    }
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
          lift(Ident(newTermName("captured$" + t.name.decoded)))
        case _ =>
          super.transform(tree)
      }
    }
  }

}