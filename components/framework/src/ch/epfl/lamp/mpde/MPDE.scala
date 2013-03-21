package ch.epfl.lamp
package yinyang

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.immutable.Map
import scala.reflect.macros.Context
import scala.collection.mutable
import language.experimental.macros

import yinyang.api._

import java.util.concurrent.atomic.AtomicLong

object YYTransformer {

  def apply[C <: Context, T](c: C, dslName: String, debug: Boolean = false, rep: Boolean = false) =
    new YYTransformer(c, dslName, debug, rep)

  protected[yinyang] val uID = new AtomicLong(0)
}

// for now configuration goes as a named parameter list
final class YYTransformer[C <: Context, T](
  val c: C,
  dslName: String,
  val debug: Boolean = false,
  val rep: Boolean = false) {
  import c.universe._

  val symbolIds: mutable.HashMap[Int, Symbol] = new mutable.HashMap()
  def symbolById(id: Int) = symbolIds(id)

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
    val ascribed = block.tree
    val ascribed1 = if (rep) {
      val ascrBody = new AscriptionTransformer().injectAscription(block.tree)
      log("Ascription: " + show(ascrBody))
      ascrBody
    } else {
      block.tree
    }

    // mark captured variables as holes
    val allCaptured = freeVariables(block.tree)
    val lifted = LiftLiteralTransformer(ascribed)
    val injected = new ScopeInjectionTransformer().transform(lifted)
    val transfBody = new HoleMarkerTransformer(allCaptured map symbolId) mark injected
    log("Transformed Body: " + showRaw(transfBody))

    // generates the Embedded DSL cake with the transformed "main" method.
    val dslClassPre = c.resetAllAttrs(composeDSL(transfBody)) // TODO this should not exist

    log(s"Pre eval: \n ${show(dslClassPre)}")
    // if the DSL inherits the StaticallyChecked trait stage it and do the static analysis
    if (dslInstance(dslClassPre).isInstanceOf[StaticallyChecked])
      dslInstance(dslClassPre).asInstanceOf[StaticallyChecked].staticallyCheck(c)

    // DSL returns what holes it needs
    val requiredVariables = dslInstance(dslClassPre).asInstanceOf[BaseYinYang].stagingAnalyze().map(symbolById)
    val canCompile = requiredVariables.isEmpty

    val holes = allCaptured diff requiredVariables

    // re-transform the tree with new holes
    val toTransform =
      if (canCompile) transfBody
      else {
        // TODO Duy this is not clean. Maybe we could make a new transformer to just flip the hole into lift.
        val lifted = LiftLiteralTransformer(ascribed, idents = requiredVariables, free = allCaptured)
        val injected = new ScopeInjectionTransformer().transform(lifted)
        val transfBody = new HoleMarkerTransformer(holes map symbolId).mark(injected)
        transfBody
      }
    log("toTransform: " + show(toTransform))
    val dslClass = c.resetAllAttrs(composeDSL(toTransform))
    log("DSL Class: " + show(dslClass))
    def args(holes: List[Symbol]): String =
      holes.map({ y: Symbol ⇒ y.name.decoded }).mkString("", ",", "")

    val dslTree = dslInstance(dslClassPre) match {
      case dsl: CodeGenerator if canCompile ⇒
        /*
       * If DSL does not require run-time data it can be completely
       * generated at compile time and wired for execution. 
       */
        val codeGenerator = dslInstance(dslClass).asInstanceOf[CodeGenerator]

        val code = s"""
          ${codeGenerator generateCode className}          
          new $className().apply(${args(allCaptured)})
        """
        log(s"generated: ${code}")
        val parsed = c parse (code)

        parsed
      case dsl: CodeGenerator ⇒
        /*
       * If DSL need runtime info send it to run-time and install a guard for re-compilation based on required symbols. 
       */
        val nameCurrent = "current"
        val nameClassName = "className"
        val nameDSLInstance = "dslInstance"
        val nameDSLProgram = "dslProgram"
        val nameCompileStorage = "__compiledStorage"
        val nameRecompile = "recompile"

        val typeT = TypeTree(block.tree.tpe)

        val valCurrent =
          c parse s"val $nameCurrent: List[Any] = List(${requiredVariables map (_.name.decoded) mkString ", "})"

        val valDslInstance = c parse s"val $nameDSLInstance = new $className()"

        val retTypeTree: Tree = TypeTree(block.tree.tpe)

        val argsCnt = args(holes).length
        val functionTypeTree =
          AppliedTypeTree(Select(Select(Ident("_root_"), "scala"), newTypeName("Function" + argsCnt)),
            (0 until argsCnt).map(y ⇒ Ident(newTermName("scala.Any"))).toList ::: List(retTypeTree))

        val recompileF = c parse s"def $nameRecompile(): () => Any = $nameDSLInstance.compile[Int]" match {
          case DefDef(mods, name, tparams, vparamss, AppliedTypeTree(function0, _), TypeApply(compile, _)) ⇒
            DefDef(mods, name, tparams, vparamss, AppliedTypeTree(function0, List(typeT)), TypeApply(compile, List(typeT, functionTypeTree)))
        }

        val guard = c parse s"""val $nameDSLProgram = 
        $nameCompileStorage.checkAndUpdate[Int](${new scala.util.Random().nextInt}, $nameCurrent, $nameRecompile)""" match {
          case ValDef(modes, name, tp, Apply(TypeApply(check, _), args)) ⇒
            ValDef(modes, name, tp, Apply(TypeApply(check, List(functionTypeTree)), args))
        }

        val DSL = c parse s"$nameDSLProgram.apply(${args(holes)})"
        val finalBlock = Block(
          dslClass,
          valCurrent,
          valDslInstance,
          recompileF,
          guard,
          DSL)

        log("Guarded block:" + show(finalBlock))
        finalBlock
      case dsl: Interpreted ⇒
        // in this case we just call the interpret method
        val retTypeTree: Tree = TypeTree(block.tree.tpe)
        val argsCnt = args(holes).length

        Block(dslClass,
          Apply(
            TypeApply(
              Select(constructor, newTermName(interpretMethod)),
              List(retTypeTree)),
            holes.map(Ident(_))))
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
        if (sym.isTerm && !(sym.isMethod || sym.isPackage || sym.isModule) && isFree(sym)) collected append sym
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

  private object LiftLiteralTransformer {
    def apply(tree: Tree, idents: List[Symbol] = Nil, free: List[Symbol] = Nil) =
      new LiftLiteralTransformer(idents, free)(tree)
  }

  private final class LiftLiteralTransformer(val idents: List[Symbol], val free: List[Symbol]) extends Transformer {

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(v)) ⇒
          Apply(Select(This(newTypeName(className)), newTermName("lift")), List(t))
        case t @ Ident(v) if free.contains(t.symbol) && idents.contains(t.symbol) ⇒
          Apply(Select(This(newTypeName(className)), newTermName("lift")), List(t))
        case _ ⇒

          super.transform(tree)
      }
    }

    def apply(tree: Tree) = transform(tree)
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
            TypeApply(Select(This(newTypeName(className)), newTermName(holeMethod)), List(TypeTree(), TypeTree())),
            //            List(Apply(TypeApply(Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("manifest")), List(TypeTree(i.tpe))), List()),
            List(Apply(TypeApply(Ident(newTermName("manifest")), List(TypeTree(i.tpe))), List()),
              Literal(Constant(id))))
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
        //provide Def trees with NoSymbol (for correct show(tree)
        case vdDef: ValOrDefDef ⇒ {
          val retDef = super.transform(tree)
          retDef.setSymbol(NoSymbol)
          retDef
        }

        case typTree: TypTree if typTree.tpe != null ⇒
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
          if (!featureExists("__ifThenElse", List(cond.tpe, then.tpe, elze.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the conditionnal branches!")
          }
          method("__ifThenElse", List(transform(cond), transform(then), transform(elze)))

        // Variable definition, assignment and return : VariableEmbeddingDSL
        case ValDef(mods, sym, tpt, rhs) ⇒ // TODO This does var and val definition lifting. Is that OK ?
          if (!featureExists("__newVar", List(rhs.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the creation of variables!")
          }
          ValDef(mods, sym, transform(tpt), method("__newVar", List(transform(rhs)))) // If there is a type transformer, it would be good to transform also the type tree

        case Return(e) ⇒
          if (!featureExists("__return", List(e.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the return of values!")
          }
          method("__return", List(transform(e)))

        case Assign(lhs, rhs) ⇒
          if (!featureExists("__assign", List(lhs.tpe, rhs.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the assignment!")
          }
          method("__assign", List(transform(lhs), transform(rhs)))

        // While and DoWhile: ImperativeDSL
        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label), List())), Literal(Constant()))) if label == sym ⇒ // While
          if (!featureExists("__whileDo", List(cond.tpe, body.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the while loops!")
          }
          method("__whileDo", List(transform(cond), transform(body)))
        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label), List()), Literal(Constant())))) if label == sym ⇒ // DoWhile
          if (!featureExists("__doWhile", List(body.tpe, cond.tpe))) {
            c.error(tree.pos, "The DSL doesn't support the do-while loops!")
          }
          method("__doWhile", List(transform(body), transform(cond)))

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
  val className = "generated$" + dslName.filter(_ != '.') + YYTransformer.uID.incrementAndGet
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

  def method(methName: String, args: List[Tree]) =
    Apply(Select(This(newTypeName(className)), newTermName(methName)), args)

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

  def featureExists(feature: String, args: List[Type]): Boolean = featuresExist(Set((None, feature, Nil, args)))

  def featuresExist(feature: Set[(Option[Type], String, List[Tree], List[Type])]): Boolean = {

    def application(tpe: Option[Type], method: String, typeArgs: List[Tree], args: List[Type]): Tree = {
      def dummyTree(tpe: Type) =
        TypeApply(Select(Literal(Constant(())), newTermName("asInstanceOf")), List(constructTypeTree(tpe)))

      def app = tpe match {
        case Some(tpe) ⇒ Apply(Select(dummyTree(tpe), newTermName(method)), args.map(dummyTree))
        case None      ⇒ Apply(Select(This(className), newTermName(method)), args.map(dummyTree))
      }

      typeArgs match {
        case Nil                 ⇒ app
        // TODO here we need type remapping
        case tparams: List[Tree] ⇒ TypeApply(app, tparams)
      }
    }
    val st = System.currentTimeMillis()
    val res = try {
      // block containing only dummy methods that were applied. 
      c.typeCheck(Block(composeDSL(Block(feature.map(x ⇒ application(x._1, x._2, x._3, x._4)).toSeq: _*)), Literal(Constant(()))))
      true
    } catch {
      case e: Throwable ⇒
        false
    }
    println(s"Feature checking time: ${(System.currentTimeMillis() - st)}")
    res
  }

  def log(s: String) = if (debug) println(s)

}

