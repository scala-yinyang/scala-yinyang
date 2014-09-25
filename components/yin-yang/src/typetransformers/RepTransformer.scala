package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.blackbox.Context

trait RepTransformerLike[C <: Context] extends PolyTransformerLike[C] { this: TypeTransformer[C] with PolyTransformerLike[C] =>
  import c.universe._
  val IRType = "Rep"
  override def constructPolyTree(typeCtx: TypeContext, inType: Type): Tree = inType match {
    case TypeRef(pre, sym, Nil) =>
      Ident(inType.typeSymbol.name)
    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(Ident(toType(sym)),
        args map { x => constructPolyTree(typeCtx, x) })
    case ConstantType(t) =>
      TypeTree(inType)
    case another @ _ =>
      super.constructPolyTree(typeCtx, another)
  }

  def rep(inType: Type): Tree = {
    AppliedTypeTree(Select(This(newTypeName(className)), newTypeName(IRType)),
      List(TypeTree(inType)))
  }

  /**
   * transform Type1[Type2[...]] => Rep[Type1[Type2[...]]] for non-function types
   */
  def constructRepTree(ctx: TypeContext, inType: Type): Tree = {
    val universe = c.universe.asInstanceOf[scala.reflect.internal.Types]

    ctx match {
      case TypeArgCtx | TypeParameterCtx =>
        TypeTree(inType)
      case OtherCtx =>
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
}

/**
 * Type transformer for Rep types.
 */
class RepTransformer[C <: Context](ctx: C) extends TypeTransformer[C](ctx) with RepTransformerLike[C] with PolyTransformerLike[C] {
  import c.universe._

  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree =
    constructRepTree(ctx, t)
}

/**
 * Type transformer for Pardis Rep types.
 */
class GenericTypeTransformer[C <: Context](ctx: C) extends RepTransformer[C](ctx) {
  import c.universe._

  override def constructRepTree(ctx: TypeContext, inType: Type): Tree = {
    ctx match {
      case OtherCtx =>
        inType match {
          case inType if isFunctionType(inType) =>
            rep(inType)
          case _ =>
            super.constructRepTree(ctx, inType)
        }
      case _ => super.constructRepTree(ctx, inType)
    }
  }
}

/**
 * Type transformer for Pardis Rep types.
 */
class PardisRepTransformer[C <: Context](ctx: C) extends RepTransformer[C](ctx) {
  import c.universe._

  override def rep(inType: Type): Tree = {
    val repType = varCtx match {
      case IsVar  => "Var"
      case NotVar => IRType
    }
    AppliedTypeTree(Select(This(newTypeName(className)), TypeName(repType)),
      List(TypeTree(inType)))
  }

  override def constructRepTree(ctx: TypeContext, inType: Type): Tree = {
    ctx match {
      case OtherCtx =>
        inType match {
          case inType if isFunctionType(inType) =>
            rep(inType)
          case _ =>
            super.constructRepTree(ctx, inType)
        }
      case _ => super.constructRepTree(ctx, inType)
    }
  }
}
