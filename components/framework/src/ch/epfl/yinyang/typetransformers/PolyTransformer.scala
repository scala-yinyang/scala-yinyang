package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.Context
import scala.reflect.runtime.universe.definitions.FunctionClass

trait PolyTransformerLike[C <: Context] { this: TypeTransformer[C] =>
  import c.universe._

  val MaxFunctionArity = 22
  def toType(s: Symbol) = s.name

  protected def isFunctionType(tp: Type): Boolean = tp.normalize match {
    case TypeRef(pre, sym, args) if args.nonEmpty =>
      val arity = args.length - 1

      arity <= MaxFunctionArity &&
        arity >= 0 &&
        sym.fullName == FunctionClass(arity).fullName
    case _ =>
      false
  }

  def constructPolyTree(typeCtx: TypeContext, inType: Type): Tree = inType match {

    case TypeRef(pre, sym, Nil) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case TypeRef(pre, sym, Nil) =>
      Select(This(newTypeName(className)), toType(inType.typeSymbol))

    case TypeRef(pre, sym, args) if isFunctionType(inType) =>
      AppliedTypeTree(Select(Ident(newTermName("scala")), toType(sym)),
        args map { x => constructPolyTree(OtherCtx, x) })

    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(Select(This(newTypeName(className)), toType(sym)),
        args map { x => constructPolyTree(OtherCtx, x) })

    case ConstantType(t) =>
      Select(This(newTypeName(className)), toType(inType.typeSymbol))

    case SingleType(pre, name) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case SingleType(pre, name) if inType.typeSymbol.isModuleClass =>
      SingletonTypeTree(Select(This(newTypeName(className)),
        newTermName(inType.typeSymbol.name.toString)))

    case s @ SingleType(pre, name) if inType.typeSymbol.isClass =>
      constructPolyTree(OtherCtx,
        s.asInstanceOf[scala.reflect.internal.Types#SingleType]
          .underlying.asInstanceOf[c.universe.Type])

    case another @ _ =>
      println(("!" * 10) + s"""Missed: $inType = ${
        showRaw(another)
      } name = ${inType.typeSymbol.name}""")
      TypeTree(another)
  }

}

class PolyTransformer[C <: Context](ctx: C) extends TypeTransformer[C](ctx) with PolyTransformerLike[C] {
  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree =
    constructPolyTree(ctx, t)
}