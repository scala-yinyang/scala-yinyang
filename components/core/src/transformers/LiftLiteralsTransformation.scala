package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Lifts constants by wrapping them in a lift(<lit>) and renames and lifts the
 * in toLift provided captured identifiers from outside the DSL scope by
 * wrapping them in lift(captured$<id>). The toMixed identifiers will be
 * wrapped in lift(captured$<id>, Ident(t)) and the second argument will be
 * transformed into a hole() call by the HoleTransformer.
 */
trait LiftLiteralTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(toLift: List[Symbol])(tree: Tree) = {
      val t = new LiftLiteralTransformer(toLift).transform(tree)
      log("lifted: " + t, 2)
      t
    }
  }

  class LiftLiteralTransformer(toLift: List[Symbol])
    extends Transformer {

    def lift(t: List[Tree]) = q"$$lift(..$t)"

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) => lift(List(t))

        case t @ Ident(n) =>
          if (toLift.contains(t.symbol))
            lift(List(Ident(TermName(t.name.decodedName.toString))))
          else t

        case _ => super.transform(tree)
      }
    }
  }

}
