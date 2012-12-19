package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros

object MPDE {

  // configuration params to be included
  val dslName = "VectorDSL"
  val className = "test"
    val dslMethod = "main"
  /**
   * Macro that converts pure DSL embedding into a deep one.
   */
  def lift[T](c: Context)(cake: c.Expr[String], block: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    /*
     * This body of code generates the Embedded DSL cake with the empty main method.
     */  
    // TODO main should return type T in some way.
    val cake = Block(List(
      ClassDef(Modifiers(), newTypeName(className), List(), Template(
        List(Ident(newTypeName(dslName))),
        emptyValDef,
        List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
          DefDef(Modifiers(), newTermName(dslMethod), List(), List(List()), Ident(newTypeName("Unit")), Literal(Constant(()))))))),
      Apply(Select(Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List()), newTermName(dslMethod)), List()))

    // TODO insert `block` into the main method :/    
    c.Expr[T](c.resetAllAttrs(cake))
  }

  // Transformer that will inject the body into the cake.
  private class MpdeTransformer(c: Context) {
    import c.universe._
    
    /*
     * This transformer needs to do the following:
     *   - re-wire all top-level objects to cake objects
     *   - re-wire all method selections to cake types
     *   - lift all identifiers and constants with a liftTerm method
     *   - convert all language constructs to method calls (__ifThenElse, __while, ...)
     *   - re-wire types to their cake counterparts (in case of type ascriptions and type application etc.) 
     *   ...
     */
    private class ScopeInjectionTransformer extends Transformer {

      override def transform(tree: Tree): Tree = {
        tree match {
          // TODO fill in the requirements
          case _ =>
            super.transform(tree)
        }
      }
    }
  }
}
