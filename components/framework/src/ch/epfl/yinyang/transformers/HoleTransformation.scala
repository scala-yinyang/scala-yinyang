package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.Context
import language.experimental.macros
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  val holeTable = new ArrayBuffer[Int]

  object HoleTransformer {
    def apply(toMark: List[Int] = Nil, shortenNames: Tree => String)(tree: Tree) = {
      val t = new HoleTransformer(toMark).transform(tree)
      log("holeTransformed (marking " + toMark + "): " + shortenNames(t), 2)
      t
    }
  }
  /**
   * Replace all variables in `toMark` with `hole[T](classTag[T], symbolId)`
   */
  class HoleTransformer(toMark: List[Int]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case i @ Ident(s) if toMark contains symbolId(i.symbol) => {
        val index = {
          val sId = symbolId(i.symbol)
          if (holeTable.contains(sId))
            holeTable.indexOf(sId)
          else {
            holeTable += symbolId(i.symbol)
            holeTable.size - 1
          }
        }
        Apply(
          Select(This(tpnme.EMPTY), newTermName(holeMethod)),
          List(
            TypeApply(
              Select(Select(Select(Select(Select(Ident(newTermName("scala")), newTermName("reflect")),
                newTermName("runtime")), nme.PACKAGE), newTermName("universe")),
                newTermName("typeTag")), List(TypeTree(i.tpe.widen))),
            Literal(Constant(index))))
      }
      case _ =>
        super.transform(tree)
    }
  }
}