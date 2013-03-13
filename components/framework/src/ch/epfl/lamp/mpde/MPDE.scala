package ch.epfl.lamp
package mpde

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.immutable.Map
import scala.reflect.macros.Context
import scala.collection.mutable
import language.experimental.macros

import mpde.api._

import java.util.concurrent.atomic.AtomicLong

object MPDETransformer {
  val uID = new AtomicLong(0)
}

// for now configuration goes as a named parameter list
final class MPDETransformer[C <: Context, T](
  val c: C,
  dslName: String,
  val debug: Boolean = false,
  val rep: Boolean = false) {
  import c.universe._

  val symbolIds: mutable.HashMap[Long, Symbol] = new mutable.HashMap()
  def symbolById(id: Long) = symbolIds(id)

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
    // TODO language feature check should go first and fail with error!

    // ascription according to original types
    val ascribed = if (rep) {
      val ascrBody = new AscriptionTransformer().injectAscription(block.tree)
      log("Ascription: " + show(ascrBody))
      ascrBody
    } else {
      block.tree
    }

    // mark captured variables as holes
    val allCaptured = freeVariables(block.tree)
    val allMarkedAsHoles = new HoleMarkerTransformer(allCaptured map symbolId) mark ascribed
    log(s"Body with all captured variables marked: ${allMarkedAsHoles}")

    // put the body into the cake and re-wire
    val transfBody = new ScopeInjectionTransformer().transform(allMarkedAsHoles)
    log("Transformed Body: " + show(transfBody))

    // generates the Embedded DSL cake with the transformed "main" method.
    val dslClassPre = c.resetAllAttrs(composeDSL(transfBody)) // TODO this should not exist

    // if the DSL inherits the StaticallyChecked trait stage it and do the static analysis
    if (dslInstance(dslClassPre).isInstanceOf[StaticallyChecked])
      dslInstance(dslClassPre).asInstanceOf[StaticallyChecked].staticallyCheck(c)

    // DSL returns what holes it needs
    val requiredVariables = dslInstance(dslClassPre).asInstanceOf[BaseYinYang].stagingAnalyze().map(symbolById)
    val canCompile = requiredVariables.isEmpty

    val holes = allCaptured diff requiredVariables

    // re-transform the tree with new holes
    val toTransform =
      if (canCompile) allMarkedAsHoles
      else new HoleMarkerTransformer(holes map symbolId).mark(block.tree)
    val dslClass = c.resetAllAttrs(composeDSL(
      new ScopeInjectionTransformer().transform(toTransform)))
    log("DSL Class: " + show(dslClass))

    def args(holes: List[Symbol], nameGenerator: CodeGenerator): List[String] =
      holes map (symbol ⇒ s"val ${nameGenerator.generateName(symbolId(symbol))} = ${symbol.name.decoded};\n")

    val dslTree = if (dslInstance(dslClass).isInstanceOf[CodeGenerator]) {
      val codeGenerator = dslInstance(dslClass).asInstanceOf[CodeGenerator]
      val outerScope = className + "$outer$scope"

      val generated = codeGenerator generateCode className
      // TODO (Duy) external parameter tracking.
      val enscoped =
        s"""
          object $outerScope {
            def apply() = {
              ${args(allCaptured, codeGenerator) mkString ";\n"}
              $generated
              // put an apply here
            }
          }
          $outerScope.apply()
        """
      val parsed = c parse enscoped
      log(s"generated: ${parsed.toString}")
      parsed
    } else {
      val nameGenerator = new CodeGenerator { def generateCode(className: String) = "" }
      val nameCurrent = "current"
      val nameClassName = "className"
      val nameDSLInstance = "dslInstance"
      val nameDSLProgram = "dslProgram"
      val nameCompileStorage = "__compiledStorage"
      val nameRecompile = "recompile"

      val typeT = TypeTree(block.tree.tpe)

      val valClassName = c parse (s"val $nameClassName = ${"\"" + className + "\""}")

      val valCurrent =
        c parse s"val $nameCurrent: List[Any] = List(${requiredVariables map (_.name.decoded) mkString ", "})"

      val valDslInstance = c parse s"val $nameDSLInstance = new $className()"

      val defRecompile = c parse s"def $nameRecompile(): () => Int = $nameDSLInstance.compile[Int]" match {
        case DefDef(mods, name, tparams, vparamss, AppliedTypeTree(function0, _), TypeApply(compile, _)) ⇒
          DefDef(mods, name, tparams, vparamss, AppliedTypeTree(function0, List(typeT)), TypeApply(compile, List(typeT)))
      }

      val valDslProgram = c parse s"val $nameDSLProgram =" +
        s" $nameCompileStorage.checkAndUpdate[Int]($nameClassName, $nameCurrent, $nameRecompile)" match {
          case ValDef(modes, name, tp, Apply(TypeApply(check, _), args)) ⇒
            ValDef(modes, name, tp, Apply(TypeApply(check, List(typeT)), args))
        }

      val valHoles = args(holes, nameGenerator) map c.parse

      val runDSL = c parse s"$nameDSLProgram.apply()"

      val finalBlock = Block(
        valHoles ++ List(dslClass, valClassName, valCurrent, valDslInstance, defRecompile, valDslProgram),
        runDSL)

      /*
      // this is what it looks like if written by user
      s"""
        type T = block.tree.tpe

        dslClass

        val $nameClassName = $className
        val $nameCurrent: List[Any] = List(${requiredVariables map (_.name.decoded) mkString ", "})

        val $nameDSLInstance = new $className()

        def $nameRecompile() = $nameDSLInstance.compile[T]

        // Solution for 1. in the email: we can define values for the holes here.
        ${args(holes, nameGenerator) mkString ";\n"}

        val $nameDSLProgram = $nameCompileStorage.checkAndUpdate[T]($nameClassName, $nameCurrent, $nameRecompile)

        $nameDSLProgram.apply()
      """
      */

      finalBlock
    }

    log("Final tree: " + show(c.typeCheck(c.resetAllAttrs(dslTree))))
    c.Expr[T](c.resetAllAttrs(dslTree))
  }

  def freeVariables(tree: Tree): List[Symbol] = new FreeVariableCollector().collect(tree)

  private final class FreeVariableCollector extends Traverser {

    private[this] val collected = ListBuffer[Symbol]()
    private[this] var defined = List[Symbol]()

    private[this] final def isFree(id: Symbol) = !defined.contains(id)

    override def traverse(tree: Tree) = tree match {
      case i @ Ident(s) ⇒ {
        val sym = i.symbol
        if (sym.isTerm && !(sym.isMethod || sym.isPackage) && isFree(sym)) collected append sym
      }
      case _ ⇒ super.traverse(tree)
    }

    def collect(tree: Tree): List[Symbol] = {
      collected.clear()
      defined = new LocalDefCollector().definedSymbols(tree)
      log(s"Defined: $defined")
      traverse(tree)
      collected.toList.distinct
    }

  }

  private final class LocalDefCollector extends Traverser {

    private[this] val definedValues, definedMethods = ListBuffer[Symbol]()

    override def traverse(tree: Tree) = tree match {
      case vd: ValDef ⇒ definedValues += vd.symbol
      case dd: DefDef ⇒ definedMethods += dd.symbol
      case _          ⇒ super.traverse(tree)
    }

    def definedSymbols(tree: Tree): List[Symbol] = {
      definedValues.clear()
      definedMethods.clear()
      traverse(tree)
      (definedValues ++ definedMethods).toList
    }

  }

  private def dslConstructor = constructor(className, List())

  private def dslClass(mainBody: Tree): Tree = c.resetAllAttrs(composeDSL(mainBody))

  def constructor(classname: String, arguments: List[Tree]): Tree =
    invoke(newClass(classname), nme.CONSTRUCTOR, arguments)

  private final class AscriptionTransformer extends Transformer {
    var ident = 0
    var externalApplyFound = false

    def injectAscription(tree: Tree): Tree = {
      val ascrTree = transform(tree)
      ascrTree
    }

    override def transform(tree: Tree): Tree = {
      log(" " * ident + " ==> " + tree)
      ident += 1

      val result = tree match {
        case vd @ ValDef(mods, name, tpt, rhs) ⇒ {
          val retDef = ValDef(mods, name, tpt, Typed(transform(rhs), TypeTree(tpt.tpe)))

          //TODO (TOASK) - we need way to save symbols and types
          retDef.setType(vd.tpe)
          retDef.setSymbol(vd.symbol)
          retDef
        }

        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) ⇒ {
          val retDef = DefDef(mods, name, tparams, vparamss, tpt, Typed(transform(rhs), TypeTree(tpt.tpe)))

          retDef.setType(dd.tpe)
          retDef.setSymbol(dd.symbol)
          retDef
        }

        //TODO refactor this in more functional way
        case ap @ Apply(fun, args) ⇒ {
          val ascrArgs = args map {
            x ⇒
              //TODO refactor to use isConstant
              //x ⇒ Typed(transform(x), TypeTree(if (x.symbol != null && x.symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isConstant) x.tpe.erasure else x.tpe)) //working solution
              val auniverse = c.universe.asInstanceOf[scala.reflect.internal.Types]
              log("scala.reflect.runtime.universe.asInstanceOf[scala.reflect.internal.Types].isConstantType(x.tpe) = " +
                auniverse.isConstantType(tree.tpe.asInstanceOf[auniverse.Type]))
              Typed(transform(x), TypeTree(if (x.tpe != null && auniverse.isConstantType(x.tpe.asInstanceOf[auniverse.Type])) x.tpe.erasure else x.tpe))
          }
          val retApply = if (externalApplyFound) {
            Apply(transform(fun), ascrArgs)
          } else {
            externalApplyFound = true
            val baseTree = Apply(transform(fun), ascrArgs)
            externalApplyFound = false
            Typed(baseTree, TypeTree(ap.tpe))
          }
          retApply
        }
        //
        //case lit @ Literal(Constant(_)) ⇒ {
        //  Typed(lit, TypeTree(lit.tpe.erasure))
        //}

        case _ ⇒
          super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <== " + result)

      result
    }
  }

  /**
   * Replace all variables in `toMark` with `hole[](id)`
   */
  private final class HoleMarkerTransformer(toMark: List[Int]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case i @ Ident(s) ⇒
        val id = symbolId(i)
        symbolIds.put(id, i.symbol)
        if (toMark contains id)
          Apply(
            TypeApply(Select(This(newTypeName(className)), newTermName(holeMethod)), List(TypeTree())),
            List(Literal(Constant(id))))
        else
          super.transform(tree)
      //case s @ Select(id: Ident, _) if toMark contains id.symbol ⇒
      //mark(s.symbol)
      case _ ⇒
        super.transform(tree)
    }

    def mark(tree: Tree): Tree = {
      log(s"To mark: ${toMark.toString}")
      transform(tree)
    }

  }

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

    val notLiftedTypes: Set[Type] = Set(
      c.universe.typeOf[scala.math.Numeric.IntIsIntegral.type],
      c.universe.typeOf[scala.math.Numeric.DoubleIsFractional.type],
      c.universe.typeOf[scala.reflect.ClassTag[Int]].erasure)

    def isLifted(tp: Type): Boolean =
      !(notLiftedTypes exists (_ =:= tp.erasure))

    /**
     * Current solution for finding outer scope idents.
     */
    def markDSLDefinition(tree: Tree) = tree match {
      case _: ValDef ⇒ definedValues += tree.symbol
      case _: DefDef ⇒ definedMethods += tree.symbol
      case _         ⇒
    }

    private[this] final def isFree(s: Symbol) = !(definedValues.contains(s) || definedMethods.contains(s))

    private[this] final def isHole(tree: Tree): Boolean =
      tree match {
        case Apply(
          TypeApply(Select(This(_), methodName), List(TypeTree())),
          List(Literal(Constant(_: Int)))) if methodName.decoded == holeMethod ⇒ true
        case _ ⇒ false
      }

    var ident = 0

    override def transform(tree: Tree): Tree = {
      markDSLDefinition(tree)

      log(" " * ident + " ==> " + tree)
      ident += 1

      val result = tree match {
        // lifting of literals
        case t @ Literal(Constant(v)) ⇒ // v => liftTerm(v)
          Apply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(t))

        //        // If the identifier is a val or var outside the scope of the DSL we will lift it.
        //        // This approach does no cover the case of methods with parameters. For now they will be disallowed.
        //        case t @ Ident(v) if isFree(t.symbol) && !t.symbol.isModule ⇒
        //          Apply(TypeApply(Select(This(newTypeName(className)), newTermName("liftTerm")), List(TypeTree(), TypeTree())), List(t))

        //provide Def trees with NoSymbol (for correct show(tree)
        case vdDef: ValOrDefDef ⇒ {
          val retDef = super.transform(tree)
          retDef.setSymbol(NoSymbol)
          retDef
        }

        case typTree: TypTree ⇒
          if (isLifted(typTree.tpe)) {
            constructTypeTree(typTree.tpe)
          } else {
            //TODO move it to tree construction methods (and inject to them)
            constructNotLiftedTree(typTree.tpe)
          }

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
          if (rep)
            TypeApply(transform(mth), targs)
          else {
            val liftedTargs = targs map (transform(_))
            TypeApply(transform(mth), liftedTargs)
          }

        // Removes all import statements for now. 
        // TODO later it will figure out the DSL modules and will include them into the cake.
        case Import(_, _) ⇒
          EmptyTree

        case f @ Function(params, body) ⇒
          // TODO transform ValDef types here with an explicit type tree.
          val functionParams: List[ValDef] = params map { x ⇒ transform(x).asInstanceOf[ValDef] }

          c.resetAllAttrs(f)
          Function(functionParams, transform(body))
        // re-wire language feature `if` to the method __ifThenElse
        case t @ If(cond, then, elze) ⇒
          Apply(Select(This(newTypeName(className)), newTermName("__ifThenElse")), List(transform(cond), transform(then), transform(elze)))

        // TODO partial functions, try catch, pattern matching

        // ignore the hole
        case _ if isHole(tree) ⇒ tree
        case _ ⇒
          super.transform(tree)
      }

      ident -= 1
      log(" " * ident + " <== " + result)

      result
    }
  }

  /*
  * Configuration parameters.
  */
  def interpretMethod = "interpret"
  val dslMethod: String = "main"
  val holeMethod = "hole"
  val className = "generated$" + dslName.filter(_ != '.') + MPDETransformer.uID.incrementAndGet
  def constructTypeTree(inType: Type) = if (rep)
    constructRepTree(inType)
  else
    constructPolyTree(inType)

  //TODO (TOASK) - do we need tp.normalize here?
  private def isFunctionType(tp: Type): Boolean = tp.normalize match {
    case TypeRef(pre, sym, args) if args.nonEmpty ⇒
      val arity = args.length - 1
      import scala.reflect.runtime.universe.definitions._
      val MaxFunctionArity = 22
      //TODO refactor symbol comparision
      //val myuniverse2 = c.universe.asInstanceOf[scala.reflect.internal.Definitions with scala.reflect.internal.Types]
      //log("isFunctionType = " + myuniverse2.definitions.isFunctionType(tp.asInstanceOf[myuniverse2.Type]))
      //if (myuniverse2.definitions.isFunctionType(tp.asInstanceOf[myuniverse2.Type])) {
      //log("sym.id = " + sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id)
      //log("FunctionClass(arity).id = " + FunctionClass(arity).asInstanceOf[scala.reflect.internal.Symbols#Symbol].id)
      //}
      FunctionClass(arity).fullName
      arity <= MaxFunctionArity && arity >= 0 && sym.fullName == FunctionClass(arity).fullName
    case _ ⇒
      false
  }

  def constructNotLiftedTree(inType: Type): Tree = inType match {
    case TypeRef(pre, sym, args) ⇒
      if (args.isEmpty) { //Simple type
        Select(This(newTypeName(className)), inType.typeSymbol.name)
      } else { //AppliedTypeTree
        val baseTree =
          if (!isFunctionType(inType))
            Select(This(newTypeName(className)), sym.name)
          else Select(Ident(newTermName("scala")), sym.name)
        val typeTrees = args map { x ⇒ TypeTree(x) }
        AppliedTypeTree(baseTree, typeTrees)
      }

    case q @ SingleType(pre, sym) ⇒
      Select(This(newTypeName(className)), sym.name)

    case another @ _ ⇒
      TypeTree(another)
  }

  def constructPolyTree(inType: Type): Tree = inType match {
    case TypeRef(pre, sym, args) ⇒
      if (args.isEmpty) { //Simple type
        Select(This(newTypeName(className)), inType.typeSymbol.name)
      } else { //AppliedTypeTree
        val baseTree =
          if (!isFunctionType(inType))
            Select(This(newTypeName(className)), sym.name)
          else Select(Ident(newTermName("scala")), sym.name)
        val typeTrees = args map { x ⇒ constructPolyTree(x) }
        AppliedTypeTree(baseTree, typeTrees)
      }

    case another @ _ ⇒
      TypeTree(another)
  }

  def constructRepTree(inType: Type): Tree = { //transform Type1[Type2[...]] => Rep[Type1[Type2[...]]] for non-function types
    def wrapInRep(inType: Type): Tree =
      AppliedTypeTree(Select(This(newTypeName(className)), newTypeName("Rep")), List(TypeTree(inType)))

    if (isFunctionType(inType)) {
      val TypeRef(pre, sym, args) = inType
      val typeTrees = args map { x ⇒ wrapInRep(x) }
      //we can't construnct baseTree using TypeTree(pre) - pre is only scala.type not FunctionN
      //val baseTree = TypeTree(pre) //pre = scala.type
      //using such baseTree we get val a: scala.type[generated$dsllarepVectorDSL13.this.Rep[Int], generated$dsllarepVectorDSL13.this.Rep[Int]] = ...
      val baseTree = Select(Ident(newTermName("scala")), sym.name)
      AppliedTypeTree(baseTree, typeTrees)
    } else {
      wrapInRep(inType)
    }
  }

  /*
   * Utilities.
   */
  private def symbolId(symbol: Symbol): Int =
    symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id

  private def symbolId(tree: Tree): Int =
    symbolId(tree.symbol)

  def makeConstructor(classname: String, arguments: List[Tree]): Tree =
    invoke(newClass(classname), nme.CONSTRUCTOR, arguments)

  def invoke(qualifier: Tree, method: TermName, arguments: List[Tree]): Tree =
    Apply(Select(qualifier, method), arguments)

  def newClass(classname: String) =
    New(Ident(newTypeName(classname)))

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

  private var _dslInstance: Option[Object] = None
  /**
   * Returns a once initialized DSL instance when it is needed at compile time.
   */
  private def dslInstance(dslDef: Tree) = {
    if (_dslInstance == None) {
      _dslInstance = Some(
        c.eval(c.Expr(c.resetAllAttrs(Block(dslDef, dslConstructor)))))
    }
    _dslInstance.get
  }

  private def constructor = Apply(Select(New(Ident(newTypeName(className))), nme.CONSTRUCTOR), List())

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

  def methodExists(obj: Type, methodName: String, args: List[Type]): Boolean = {

    def dummyTree(tpe: Type) = TypeApply(Select(Literal(Constant(())), newTermName("asInstanceOf")), List(constructTypeTree(tpe)))
    def application(method: String) = Apply(Select(dummyTree(obj), newTermName(methodName)), args.map(dummyTree))

    try { // this might be a performance problem later. For now it will do the work.
      c.typeCheck(Block(composeDSL(application(methodName)), Literal(Constant(()))))
      true
    } catch {
      case e: Throwable ⇒
        false
    }
  }

  def log(s: String) = if (debug) println(s)

}

