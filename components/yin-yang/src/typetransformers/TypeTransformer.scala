package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.blackbox.Context

/**
 * Base class for all type transformers.
 */
abstract class TypeTransformer[C <: Context](val c: C) {
  trait TypeContext
  case object TypeArgCtx extends TypeContext
  case object TypeParameterCtx extends TypeContext
  case object OtherCtx extends TypeContext

  trait VarContext
  case object IsVar extends VarContext
  case object NotVar extends VarContext

  var varCtx: VarContext = NotVar

  def rewiredToThis(s: String) = s == "package" || s == "Predef" // TODO DRY
  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree
  var className: String = _
}