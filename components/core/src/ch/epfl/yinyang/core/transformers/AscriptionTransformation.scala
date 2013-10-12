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
trait AscriptionTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._

  def ascriptionTransforming: Boolean
  object AscriptionTransformer extends (Tree => Tree) {
    def apply(tree: Tree) =
      if (ascriptionTransforming) {
        val t = new AscriptionTransformer().transform(tree)
        log("ascription transformed: " + t, 2)
        t
      } else
        tree
  }

  private final class AscriptionTransformer extends Transformer {
    var ident = 0
    var externalApplyFound = false

    override def transform(tree: Tree): Tree = {
      log(" " * ident + " ==> " + tree, 3)
      ident += 1

      val result = tree match {
        case vd @ ValDef(m, n, t, rhs) if rhs != EmptyTree =>
          copy(vd)(ValDef(m, n, t, Typed(transform(rhs), TypeTree(t.tpe))))

        case dd @ DefDef(m, n, tp, p, rt, rhs) =>
          copy(dd)(DefDef(m, n, tp, p, rt, Typed(transform(rhs), TypeTree(rt.tpe))))
        case CaseDef(pat: Tree, guard: Tree, body: Tree) =>
          CaseDef(pat, guard, transform(body))
        case ap @ Apply(fun, args) =>
          val ascrArgs = args map {
            x => // TODO cleanup. This can be done easier.
              val auniverse = c.universe.asInstanceOf[scala.reflect.internal.Types]
              log(s"isConstantType(x.tpe) = " +
                auniverse.isConstantType(tree.tpe.asInstanceOf[auniverse.Type]), 3)
              Typed(transform(x), TypeTree(
                if (x.tpe != null &&
                  auniverse.isConstantType(x.tpe.asInstanceOf[auniverse.Type]))
                  x.tpe.erasure
                else
                  x.tpe))
          }
          if (externalApplyFound) {
            Apply(transform(fun), ascrArgs)
          } else {
            externalApplyFound = true
            val baseTree = Apply(transform(fun), ascrArgs)
            externalApplyFound = false
            Typed(baseTree, TypeTree(ap.tpe))
          }
        case _ =>
          super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <== " + result, 3)

      result
    }
  }
}
