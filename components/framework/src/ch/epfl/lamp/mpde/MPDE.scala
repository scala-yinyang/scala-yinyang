package ch.epfl.lamp
package mpde

import scala.reflect.macros.Context
import language.experimental.macros
import mpde.api._

// for now configuration goes as a named parameter list
final class MPDETransformer[C <: Context, T](
  val c: C,
  dslName: String,
  val debug: Boolean = false) {
  import c.universe._

  /**
   * Main MPDE method. Transforms the body of the DSL, makes the DSL cake out of the body and then executes the DSL code.
   * If the DSL supports static analysis of the DSL code this method will perform it during compilation. The errors
   * will be reported to the compiler error console.
   *
   * Depending on the configuration and the analysis of static values this DSL will be compiled either at compile time,
   * if all required values are present, or at runtime.
   */
  def apply[T](block: c.Expr[T]): c.Expr[T] = {
    log("Body: " + show(block.tree))
    val transfBody = new ScopeInjectionTransformer().transform(block.tree)
    log("Transformed Body: " + show(transfBody))
    // generates the Embedded DSL cake with the transformed "main" method.
    val dslClass = c.resetAllAttrs(composeDSL(transfBody))
    log("DSL Class: " + show(dslClass))

    // if the DSL inherits the StaticallyChecked trait stage it and do the analysis
    if (staticallyCheck)
      dslInstance(dslClass).asInstanceOf[StaticallyChecked].staticallyCheck(c)

    val dslTree = if (canCompileDSL(block.tree)) {
      val code = dslInstance(dslClass).asInstanceOf[CodeGenerator].generateCode("generated$" + dslName)

      // TODO (Duy) for now we do not do any external parameter tracking.
      /* 2) Code that we generate needs to link to variables. E.g:
       *   automaton.match(variable)
       *    One solution:
       *    i) re-link the variables in the generated code to method calls with literals
       *       val parsed = ...
       *       transform(parsed)
       *       and the `transform` will do the re-linking. For example:
       *         matches(x, "abc*") => matches(hole("p1"), "abc*")
       *       where `hole` is a DSL method which will generate an identifier
       * 
       *    ii) Then we produce the following
       *         object uniqueIdName {
       *            def method(p1: String, ...) = {
       *              matches(p1, "abc*") // In real world this would be compiled of course
       *            }
       *         }
       *         method(x)
       */

      val parsed = c.parse(code)
      parsed
    } else {
      Block(dslClass,
        Apply(
          TypeApply(
            Select(Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List()), newTermName(interpretMethod)),
            List(TypeTree(block.tree.tpe))),
          List()))
    }

    log("Final tree: " + show(c.typeCheck(c.resetAllAttrs(dslTree))))
    c.Expr[T](c.resetAllAttrs(dslTree))
  }

  /**
   * 1) TODO (Duy) Should be analyze(block)
   *  * true if regex is known at compile time across the whole DSL
   *     * match(variable, "abc*")
   *  * false otherwise
   *
   * The analysis requires to find methods in the DSL (cake of the DSL).
   * This should be implemented as a module so it can be reused.
   *
   */
  def canCompileDSL(body: Tree): Boolean = dslName match {
    case "dsl.print.PrintDSL" ⇒ true
    case _                    ⇒ false
  }

  /*
   * TODO (Duy)
   * 3) Enabling static analysis of the DSLs at compile time.
   *
   */
  def staticallyCheck = false

  // configuration params to be included in the end
  def className = "test"
  def interpretMethod = "interpret"
  def stageMethod = "stage"
  val dslMethod: String = "main"

  private lazy val dslTrait = {
    val names = dslName.split("\\.").toList.reverse
    assert(names.length >= 1, "DSL trait name must be in the valid format. DSL trait name is " + dslName)

    val tpeName = newTypeName(names.head)
    names.tail.reverse match {
      case head :: tail ⇒
        Select(tail.foldLeft[Tree](Ident(newTermName(head)))((tree, name) ⇒ Select(tree, newTermName(name))), tpeName)
      case Nil ⇒
        Ident(tpeName)
    }
  }

  var _dslInstance: Option[Object] = None
  /**
   * Returns a once initialized DSL instance when it is needed at compile time.
   */
  def dslInstance(dslDef: Tree) = {
    if (_dslInstance == None) {
      _dslInstance = Some(
        c.eval(
          c.Expr(
            Block(
              dslDef,
              Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List())))))
    }
    _dslInstance.get
  }

  def composeDSL(transformedBody: Tree) =
    // class MyDSL extends DSL {
    ClassDef(Modifiers(), newTypeName(className), List(), Template(
      List(dslTrait),
      emptyValDef,
      List(
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
          Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
        // def main = {
        DefDef(Modifiers(), newTermName(dslMethod), List(), List(List()), Ident(newTypeName("Any")), transformedBody))))
  //     }
  // }

  /*
   * This transformer needs to do the following:
   *   - lift all identifiers and constants with a liftTerm method
   *   - rewire all top-level objects to DSL objects
   *   - rewire all method selections to DSL method
   *   - convert all language constructs to method calls (__ifThenElse, __while, ...)
   *   - rewire types to their cake counterparts (in case of type ascription and type application etc.)
   *   ...
   */
  private final class ScopeInjectionTransformer extends Transformer {

    private val definedValues, definedMethods = collection.mutable.HashSet[Symbol]()

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
        case s @ Select(Select(inn, t: TermName), name) if s.symbol.isMethod && t.toString == "package" /* ugh, no TermName extractor */ ⇒
          Ident(name)

        case s @ Select(Select(inn, t: TermName), name) if s.symbol.isMethod && t.toString == "this" /* ugh, no TermName extractor */ ⇒
          Ident(name)

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

