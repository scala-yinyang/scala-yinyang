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
    var ident = ""
    private[this] def withCtx[T](ctx: TypeContext)(t: => T): T = {
      val currentCtx = typeCtx
      typeCtx = ctx
      val res = t
      typeCtx = currentCtx
      res
    }

    override def transform(tree: Tree): Tree = {

      log(s"$ident::> $tree", 3)
      ident += " "
      val result = tree match {
        case typTree: TypTree if typTree.tpe != null =>
          log(s"tp: ${showRaw(typTree)}", 3)
          constructTypeTree(typeCtx, typTree.tpe)

        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          varCtx = IsVar
          val newTpt = transform(tpt)
          varCtx = NotVar
          ValDef(mods, sym, newTpt, transform(rhs))

        case DefDef(mods, nme, params, args, tpt, body) =>
          val transformedParams = params map (v => withCtx(TypeParameterCtx)(transform(v).asInstanceOf[TypeDef]))
          val transformedArgs = args.map(_.map(v => transform(v).asInstanceOf[ValDef]))
          DefDef(mods, nme, transformedParams, transformedArgs, transform(tpt), transform(body))

        case TypeApply(mth, targs) =>
          val liftedArgs = targs map (v => withCtx(TypeArgCtx)(transform(v)))
          TypeApply(transform(mth), liftedArgs)

        case _ =>
          super.transform(tree)
      }

      ident = ident.tail
      log(s"$ident<:: ${showRaw(result)}", 3)
      result
    }
  }

  def constructTypeTree(tctx: TypeContext, inType: Type): Tree =
    typeTransformer transform (tctx, inType)
}