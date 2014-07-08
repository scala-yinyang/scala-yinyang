package ch.epfl.yinyang

import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import ch.epfl.yinyang.typetransformers.TypeTransformer
import scala.reflect.macros.blackbox.Context

trait TypeTreeTransformation extends MacroModule with TransformationUtils with DataDefs {
  import c.universe._
  val typeTransformer: TypeTransformer[c.type]
  import typeTransformer._

  object TypeTreeTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = {
      val t = new TypeTreeTransformer().transform(tree)
      log("typeTreeTransformed: " + code(t), 2)
      t
    }
  }

  final class TypeTreeTransformer extends Transformer {

    var typeCtx: TypeContext = OtherCtx
    var ident = 0

    override def transform(tree: Tree): Tree = {
      log(" " * ident + " ::> " + tree, 3)
      ident += 1
      val result = tree match {
        case x @ UnstageBlock(_) => x
        case typTree: TypTree if typTree.tpe != null =>
          log(s"TypeTree for ${showRaw(typTree)}", 3)
          constructTypeTree(typeCtx, typTree.tpe)

        case TypeApply(mth, targs) =>
          // TypeApply params need special treatment
          typeCtx = TypeApplyCtx
          val liftedArgs = targs map (transform(_))
          typeCtx = OtherCtx
          TypeApply(transform(mth), liftedArgs)

        case _ =>
          super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <:: " + result, 3)
      result
    }
  }

  def constructTypeTree(tctx: TypeContext, inType: Type): Tree =
    typeTransformer transform (tctx, inType)
}