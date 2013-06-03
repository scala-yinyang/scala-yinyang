package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable

/**
 * Converts captured variables to holes.
 *
 * Features covered are:
 *   - identifiers
 *   - fields (TODO)
 *   - no parameter methods (TODO)
 *   - no parameter functions (TODO)
 */
trait HoleTransformation extends MacroModule with TransformationUtils with YYConfig {

  def holeMethod: String

  import c.universe._

  object HoleTransformer {
    def apply(toMark: List[Int] = Nil)(tree: Tree) =
      new HoleTransformer(toMark).transform(tree)
  }
  /**
   * Replace all variables in `toMark` with `hole[T](classTag[T], symbolId)`
   */
  class HoleTransformer(toMark: List[Int]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case i @ Ident(s) if toMark contains symbolId(i.symbol) =>
        Apply(
          Select(This(tpnme.EMPTY), newTermName(holeMethod)),
          List(
            TypeApply(
              Select(Select(Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")),
                newTermName("runtime")), nme.PACKAGE), newTermName("universe")),
                newTermName("typeTag")), List(TypeTree(i.tpe))),
            Literal(Constant(symbolId(i.symbol)))))
      case _ =>
        super.transform(tree)
    }
  }
}