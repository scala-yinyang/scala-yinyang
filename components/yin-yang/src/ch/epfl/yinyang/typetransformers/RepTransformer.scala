package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.Context

trait RepTransformerLike[C <: Context] { this: TypeTransformer[C] with PolyTransformerLike[C] =>
  import c.universe._

  /**
   * transform Type1[Type2[...]] => Rep[Type1[Type2[...]]] for non-function types
   */
  def constructRepTree(ctx: TypeContext, inType: Type): Tree = {
    val universe = c.universe.asInstanceOf[scala.reflect.internal.Types]

    def rep(inType: Type): Tree = {
      AppliedTypeTree(Select(This(newTypeName(className)), newTypeName("Rep")),
        List(constructPolyTree(ctx, inType))) // TypeTree(inType)
    }

    inType match {
      case inType if isFunctionType(inType) =>
        val TypeRef(pre, sym, args) = inType
        val retTyperees = args map { x => rep(x) }
        //we can't construnct baseTree using TypeTree(pre) - pre is only scala.type not FunctionN
        //val baseTree = TypeTree(pre) //pre = scala.type
        //using such baseTree we get val a: scala.type[Rep[Int], Rep[Int]] = ...
        val baseTree = Select(Ident(newTermName("scala")), sym.name)
        AppliedTypeTree(baseTree, retTyperees)

      case SingleType(pre, name) if inType.typeSymbol.isClass && (!inType.typeSymbol.isModuleClass) =>
        rep(inType)

      case inType if universe.isSingleType(inType.asInstanceOf[universe.Type]) =>
        constructPolyTree(ctx, inType)

      case _ =>
        rep(inType)
    }
  }
}

/**
 * Type transformer for Rep types.
 */
class RepTransformer[C <: Context](ctx: C) extends TypeTransformer[C](ctx) with RepTransformerLike[C] with PolyTransformerLike[C] {
  import c.universe._

  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree =
    constructRepTree(ctx, t)
}