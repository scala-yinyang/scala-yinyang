package ch.epfl.yinyang.typetransformers

import scala.reflect.macros.blackbox.Context

/**
 * Base class for all type transformers.
 */
abstract class TypeTransformer[C <: Context](val c: C) {
  trait TypeContext
  case object TypeApplyCtx extends TypeContext
  case object OtherCtx extends TypeContext

  trait VarContext
  case object IsVar extends VarContext
  case object NotVar extends VarContext

  var varCtx: VarContext = NotVar

  def rewiredToThis(s: String) = s == "package" || s == "Predef" // TODO DRY
  def transform(ctx: TypeContext, t: c.universe.Type): c.universe.Tree
  var className: String = _
}

object Macros {
  def repCheck[T: c.WeakTypeTag](c: Context)(term: c.Expr[T]): c.Expr[String] = {
    import c.universe._
    val transformer = new RepTransformer[c.type](c)
    val tpe = c.weakTypeTag[T].tpe
    println("The type is: " + showRaw(tpe.normalize))
    // transformer.transform(transformer.OtherCtx, c.weakTypeTag[T].tpe)
    c.Expr[String](q"""${tpe.toString}""")
  }
}