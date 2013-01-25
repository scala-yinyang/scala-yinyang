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

    //    {class MyDSL extends DSL { }}

    //    scala> reflect.runtime.universe.showRaw(t.tree)
    //    val res15 = Block(List(
    //      ClassDef(Modifiers(), newTypeName("MyDSL"), List(), Template(
    //        List(Ident(newTypeName("DSL"))),
    //        emptyValDef,
    //        List(
    //          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
    //            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))),
    //      Literal(Constant(())))

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
            //            block.tree))))), //it was for my test
            new ScopeInjectionTransformer().transform(block.tree)))))),
      //     }
      // }
      // new MyDSL().interpret()
      Apply(Select(Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List()), newTermName(interpretMethod)), List()))

    log("Block:" + show(block, printTypes = true))
    log("Raw Block:" + showRaw(block))
    log("Cake: " + show(cake))
    log("Raw Cake: " + showRaw(cake))
    log("Type Cake: " + show(cake /*, printTypes = true*/ ))

    //TODO (TO ASK) why do we need it
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
      case _: ValDef ⇒ definedValues += tree.symbol
      case _: DefDef ⇒ definedMethods += tree.symbol
      case _         ⇒
    }

    private[this] final def isFree(s: Symbol) = !(definedValues.contains(s) || definedMethods.contains(s))

    var ident = 0

    override def transform(tree: Tree): Tree = {
      markDSLDefinition(tree)

      log(" " * ident + " ==> " + tree)
      ident += 1

      val result = tree match {
        // lifting of literals
        case t @ Literal(Constant(v)) ⇒ // v => liftTerm(v)
          Apply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(t))

        // If the identifier is a val or var outside the scope of the DSL we will lift it.
        // This approach does no cover the case of methods with parameters. For now they will be disallowed.
        case t @ Ident(v) if isFree(t.symbol) && !t.symbol.isModule ⇒
          Apply(TypeApply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))

        // re-wire objects
        case s @ Select(inn, name) if s.symbol.isMethod ⇒
          Select(transform(inn), name)

        // replaces objects with their cake counterparts
        case s @ Select(inn, name) ⇒ // TODO this needs to be narrowed down if s.symbol.isModule =>
          Ident(name)

        case TypeApply(mth, targs) ⇒ // TODO this needs to be changed for LMS to include a type transformer
          transform(mth)

        // Removes all import statements for now. TODO later it will figure out the DSL modules and will include them into the cake. 
        case Import(_, _) ⇒
          EmptyTree

        // TODO does not work because resetAllAttrs does not remove types from lambdas.
        case f @ Function(params, body) ⇒
          // TODO for LMS we will put here an explicit type for all the arguments to avoid 
          // inferencer errors.
          // For polymorphic embedding we will just null it out.
          log("Function type: " + f.symbol.typeSignature.toString)
          log("Argument type: " + params.head.symbol.typeSignature.toString)
          c.resetAllAttrs(f) // this does not re-infer the type. Why?

        // re-wire language feature `if` to the method __ifThenElse 
        case t @ If(cond, then, elze) ⇒
          Apply(Select(This(newTypeName(className)), newTermName("__ifThenElse")), List(transform(cond), transform(then), transform(elze)))

        // TODO var, while, do while, return, partial functions, try catch, pattern matching

        case _ ⇒
          super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <== " + result)

      result
    }
  }

  def log(s: String) = if (debug) println(s)

}

