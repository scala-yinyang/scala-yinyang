package ch.epfl.yinyang
package transformers

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import scala.reflect.macros.blackbox.Context
import language.experimental.macros

trait ScopeInjection extends MacroModule with TransformationUtils {
  import c.universe._
  import internal.decorators._

  val rewireThis: Boolean = false

  // TODO DRY
  def rewiredToThis(s: String) = s == "package" || s == "Predef"
  object ScopeInjectionTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      val t = new ScopeInjectionTransformer().transform(tree)
      log("scopeInjected: " + t, 2)
      t
    }
  }

  class ScopeInjectionTransformer extends Transformer {

    var ident = 0

    def preservePosition(newTree: Tree, oldTree: Tree): Tree = {
      newTree.setPos(oldTree.pos)
      newTree
    }

    // Note: the Scala compiler sometimes leaves ".this" in the path and we remove it.
    def injectModule(t: Tree): Tree =
      Ident(TermName(t.toString.replaceAll("\\.this", "").replaceAll("`package`", "package")).encodedName)

    /*
     * Translation rules:
     *   [[x.y.z.obj.method]] ~> this.`x.y.z.obj`.method
     */
    override def transform(tree: Tree): Tree = {
      log(" " * ident + " --> " + tree, 3)
      ident += 1

      val result = tree match {
        case s @ Select(inn, name) if inn.symbol.isPackage && s.symbol.isModule => injectModule(s)
        case s @ Select(inn, name) if inn.symbol.isModule => Select(transform(inn), name)
        case Apply(Select(This(typeNames.EMPTY), TermName("lift")), _) => tree
        case _ => super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <-- " + result, 3)

      preservePosition(result, tree)
    }
  }

}
