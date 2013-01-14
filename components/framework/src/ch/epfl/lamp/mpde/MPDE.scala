package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros

// for now config goes as named parameter list
final class MPDETransformer[C <: Context, T](val c: C, dslName: String, val debug: Boolean = false) {
  import c.universe._

  // configuration params to be included in the end
  def className = "test"
  def dslMethod = "main"
  def interpretMethod = "interpret"

  def apply[T](block: c.Expr[T]): c.Expr[T] = {
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
            // transformed body
            new ScopeInjectionTransformer().transform(block.tree)))))),
      //     }
      // }
      // new MyDSL().interpret()
      Apply(Select(Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List()), newTermName(interpretMethod)), List()))

    log("Block:" + show(block, printTypes = true))
    log("Raw Block:" + showRaw(block))
    log("Cake: " + show(cake))
    log("Raw Cake: " + showRaw(cake))
    log("Type Cake: " + show(cake, printTypes = true))

    c.Expr[T](c.resetAllAttrs(cake))
  }

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

    private val definedValues = collection.mutable.HashSet[Symbol]()
    private val definedMethods = collection.mutable.HashSet[Symbol]()

    /**
     * Current solution for finding outer scope idents.
     */
    def markDSLDefinition(tree: Tree) = tree match {
      case _: ValDef => definedValues += tree.symbol
      case _: DefDef => definedMethods += tree.symbol
      case _ =>
    }

    private[this] final def isFree(s: Symbol) = !(definedValues.contains(s) || definedMethods.contains(s))

    override def transform(tree: Tree): Tree = {
      markDSLDefinition(tree)

      tree match {
        // lifting of literals
        case t @ Literal(Constant(v)) => // v => liftTerm(v)
          Apply(TypeApply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))

        // If the identifier is a val or var outside the scope of the DSL we will lift it.
        // This approach does no cover the case of methods with parameters. For now they will be disallowed.
        case t @ Ident(v) if isFree(t.symbol) =>
          Apply(TypeApply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))
        case _ =>
          super.transform(tree)
      }
    }
  }

  def log(s: String) = if (debug) println(s)

}

