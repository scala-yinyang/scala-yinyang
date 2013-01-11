package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros

object MPDE {

  // configuration params to be included in the end
  val dslName = "VectorDSL"
  val className = "test"
  val dslMethod = "main"
  val interpretMethod = "interpret"

  /**
   * Macro that converts pure DSL embedding into a deep one.
   */
  // TODO allow configuration
  def lift[T](c: Context)(cake: c.Expr[String], block: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    /*
     * This body of code generates the Embedded DSL cake with the empty main method.
     */
    // Task 1: main should return type T in some way. Type inference should take care of it. However the run method should return that value. 
    // Task 2: Specify how to rewire object accesses. 
    // Task 3: Specify how to rewire constructors. 
    // Task 4: Specify how to rewire primitive types. 
    // Task 5: Specify how to rewire language features (if then else, while, try catch, return, var etc.).
    val cake = Block(List(
      // class MyDSL extends DSL {
      ClassDef(Modifiers(), newTypeName(className), List(), Template(
        List(Ident(newTypeName("VectorDSL"))),
        emptyValDef,
        List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
          // def main = {
          DefDef(Modifiers(), newTermName(dslMethod), List(), List(List()), TypeTree(),
            // TODO fill with a transformed body. 
            // Do we have access to `this` here?
            //   * Yes. This is just a `This` node with a name.
            // Can we put a block transformer here?
            // Yes. The transformer should take the name of a class for use in this. 
            // How is path dependent stuff done? How are objects and classes referenced. 
            // Again through This. Instead of the selects through objects it goes through the

            // println just to check that it works
            // Apply(Select(Select(Ident("scala"), newTermName("Predef")), newTermName("println")), List(Literal(Constant("interpreting main!!!"))))
            new MPDETransformer[c.type](c, className)(block.tree)))))),
      //     }
      // }
      // new MyDSL().interpret()
      Apply(Select(Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List()), newTermName(interpretMethod)), List()))

    println("Block:" + show(block, printTypes = true))
    println("Raw Block:" + showRaw(block))
    println("Cake: " + show(cake))
    println("Raw Cake: " + showRaw(cake))
    println("Type Cake: " + show(cake, printTypes = true))
    //    val typedCake = c.typeCheck(c.resetAllAttrs(cake))
    // TODO insert `block` into the main method :/
    //    println("Type Cake: " + show(typedCake, printTypes = true))
    c.Expr[T](c.resetAllAttrs(cake))
  }
}

// Transformer that will inject the body into the cake.
private[mpde] final class MPDETransformer[C <: Context](val c: C, cake: String) {
  import c.universe._

  /*
     * This transformer needs to do the following:
     *   - lift all identifiers and constants with a liftTerm method
     *   - rewire all top-level objects to cake objects
     *   - rewire all method selections to cake types
     *   - convert all language constructs to method calls (__ifThenElse, __while, ...)
     *   - rewire types to their cake counterparts (in case of type ascriptions and type application etc.) 
     *   ...
     */
  private final class ScopeInjectionTransformer extends Transformer {

    override def transform(tree: Tree): Tree = {
      tree match {
        // lifting of literals          
        case t @ Literal(Constant(v)) => // v => liftTerm(v)
          Apply(TypeApply(Select(This(newTypeName(cake)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))

        // if identifier is a val or var outside the scope of the DSL.
        // we can do this in the following ways:
        //  * check if ident is val or var from the outer scope (do not know how)
        //  * just lift it if it is not converted to selects
        case t @ Ident(v) =>// TODO we need to figure out where is the identifier coming from. 
          //Apply(TypeApply(Select(This(newTypeName(cake)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))
          println("Symbol: " + t.symbol + " owner:" + t.symbol.owner)
          t
        case _ =>
          super.transform(tree)
      }
    }
  }

  def apply(tree: Tree): Tree = {
    new ScopeInjectionTransformer().transform(tree)
  }
}

