package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe.definitions.FunctionClass

trait PolyTransformerLike[C <: Context] { this: TypeTransformer[C] =>
  import c.universe._

  val MaxFunctionArity = 22
  def toType(s: Symbol) = s.name

  protected def isFunctionType(tp: Type): Boolean = tp.dealias match {
    case TypeRef(_, sym, args) if args.nonEmpty =>
      val arity = args.length - 1

      arity <= MaxFunctionArity &&
        arity >= 0 &&
        sym.fullName == FunctionClass(arity).fullName
    case _ =>
      false
  }

  def constructPolyTree(typeCtx: TypeContext, inType: Type): Tree = inType match {

    case TypeRef(pre, sym, Nil) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(typeNames.EMPTY))

    case TypeRef(pre, sym, Nil) =>
      Select(This(TypeName(className)), toType(inType.typeSymbol))

    case TypeRef(pre, sym, args) if isFunctionType(inType) =>
      AppliedTypeTree(Select(Ident(TermName("scala")), toType(sym)),
        args map { x => constructPolyTree(typeCtx, x) })

    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(Select(This(TypeName(className)), toType(sym)),
        args map { x => constructPolyTree(typeCtx, x) })

    case ConstantType(t) =>
      Select(This(TypeName(className)), toType(inType.typeSymbol))

    case SingleType(pre, name) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(typeNames.EMPTY))

    case SingleType(pre, name) if inType.typeSymbol.isModuleClass =>
      SingletonTypeTree(Select(This(TypeName(className)),
        TermName(inType.typeSymbol.name.toString)))

    case s @ SingleType(pre, name) if inType.typeSymbol.isClass =>
      constructPolyTree(typeCtx,
        s.asInstanceOf[scala.reflect.internal.Types#SingleType]
          .underlying.asInstanceOf[c.universe.Type])
    case annTpe @ AnnotatedType(annotations, underlying) =>
      constructPolyTree(typeCtx, underlying)
    case another @ _ =>
      TypeTree(another)
  }

}

class PolyTransformer[C <: Context](ctx: C) extends TypeTransformer[C](ctx) with PolyTransformerLike[C] {
  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree =
    constructPolyTree(ctx, t)
}