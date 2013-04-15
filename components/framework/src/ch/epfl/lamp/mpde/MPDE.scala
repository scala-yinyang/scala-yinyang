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

object Debug {
  def show[T](x: => T): T = macro showImpl[T]
  def showImpl[T](c: Context)(x: c.Expr[T]): c.Expr[T] = {
    println(c.universe.show(x.tree))
    println(c.universe.showRaw(x.tree))
    x
  }
}

object YYTransformer {

  def apply[C <: Context, T](c: C, dslName: String,
                             shallow: Boolean = true,
                             debug: Boolean = false,
                             rep: Boolean = false,
                             refCheck: Boolean = false) =
    new YYTransformer(c, dslName, shallow, debug, rep)

  protected[yinyang] val uID = new AtomicLong(0)
}

// for now configuration goes as a named parameter list
final class YYTransformer[C <: Context, T](
  val c: C,
  dslName: String,
  val shallow: Boolean = false,
  val debug: Boolean = false,
  val rep: Boolean = false,
  val mainMethod: String = "main") {
  import c.universe._

  val symbolIds: mutable.HashMap[Int, Symbol] = new mutable.HashMap()
  def symbolById(id: Int) = symbolIds(id)

  /**
   * Main YinYang method. Transforms the body of the DSL, makes the DSL cake out
   * of the body and then executes the DSL code. If the DSL supports static
   * analysis of the DSL code this method will perform it during compilation.
   * The errors will be reported to the compiler error console.
   *
   * Depending on the configuration and the analysis of static values this DSL
   * will be compiled either at compile time, if all required values are present,
   * or at runtime.
   */
  def apply[T](block: c.Expr[T]): c.Expr[T] = {
    log("Body: " + show(block.tree))
    // shallow or detect a non-existing feature => return the original block.
    if (!FeatureAnalyzer(block.tree) || shallow)
      block
    else { // ascription according to original types
      val ascribed = if (rep) { // TODO Remove this.
        val ascrBody = new AscriptionTransformer().injectAscription(block.tree)
        log("Ascription: " + show(ascrBody))
        ascrBody
      } else {
        block.tree
      }

      // mark captured variables as holes
      val allCaptured = freeVariables(block.tree)
      val lifted = LiftLiteralTransformer(ascribed)
      val injected = ScopeInjectionTransformer(lifted)
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
          val injected = ScopeInjectionTransformer(lifted)
          val transfBody = new HoleMarkerTransformer(holes map symbolId).mark(injected)
          transfBody
        }
      log("toTransform: " + show(toTransform))
      val dslClass = c.resetAllAttrs(composeDSL(toTransform))
      log("DSL Class: " + show(dslClass))
      def args(holes: List[Symbol]): String =
        holes.map({ y: Symbol => y.name.decoded }).mkString("", ",", "")

      val dslTree = dslInstance(dslClassPre) match {
        case dsl: CodeGenerator if canCompile =>
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
          c parse (code)

        case dsl =>
          /*
       * If DSL need runtime info send it to run-time and install a guard for re-compilation based on required symbols.
       */
          val programId = new scala.util.Random().nextLong
          val retType: String = block.tree.tpe.toString
          val functionType = s"""${(0 until args(holes).length).map(y => "scala.Any").mkString("(", ", ", ")")} => ${retType}"""
          val refs = requiredVariables filterNot (isPrimitive)
          val dslInit = s"""
          val dslInstance = ch.epfl.lamp.yinyang.runtime.YYStorage.lookup(${programId}L, new $className())
          val values: Seq[Any] = Seq(${(requiredVariables diff refs) map (_.name.decoded) mkString ", "})
          val refs: Seq[Any] = Seq(${refs map (_.name.decoded) mkString ", "})
          """

          val guardedExecute = c parse dslInit + (dsl match {
            case _: CodeGenerator => s"""
            def recompile(): () => Any = dslInstance.compile[$retType, $functionType]
            val program = ch.epfl.lamp.yinyang.runtime.YYStorage.$guardType[$functionType](${programId}L, values, refs, recompile)
            program.apply(${args(holes)})
            """
            case _: Interpreted => s"""
            def invalidate(): () => Any = () => dslInstance.reset
            ch.epfl.lamp.yinyang.runtime.YYStorage.$guardType[Any](${programId}L, values, refs, invalidate)
            dslInstance.interpret[${retType}](${args(holes)})
            """
          })

          val finalBlock = Block(dslClass, guardedExecute)
          log("Guarded block:" + show(finalBlock))
          finalBlock
      }

      log("Final tree untyped: " + show(c.resetAllAttrs(dslTree)))
      log("Final tree: " + show(c.typeCheck(c.resetAllAttrs(dslTree))))
      c.Expr[T](c.resetAllAttrs(dslTree))
    }
  }

  def rewiredToThis(s: String) = s == "package" || s == "Predef"
  def freeVariables(tree: Tree): List[Symbol] = new FreeVariableCollector().collect(tree)

  object FeatureAnalyzer {
    def apply(tree: Tree, lifted: Seq[MethodInv] = Seq()): Boolean = {
      val (virtualized, lifted) = VirtualizationTransformer(tree)
      val st = System.currentTimeMillis()
      val res = new FeatureAnalyzer(lifted).analyze(virtualized)
      log(s"Feature checking time: ${(System.currentTimeMillis() - st)}")
      res
    }

  }

  private final class FeatureAnalyzer(val lifted: Seq[MethodInv]) extends Traverser {

    var methods = mutable.LinkedHashSet[MethodInv]()

    var parameterLists: List[List[Type]] = Nil

    def addIfNotLifted(m: MethodInv) =
      if (!lifted.exists(x => x.name == m.name)) {
        log(s"adding ${m}")
        methods += m
      }

    // to skip object's method invocation
    def isChecked(tree: Tree): Boolean =
      tree.symbol != null && tree.symbol.isModule

    def getObjType(obj: Tree): Type = {
      val universe = c.universe.asInstanceOf[scala.reflect.internal.Types]
      val tp: Type =
        // if (obj.symbol != null && obj.symbol.isTerm && universe.isSingleType(obj.tpe.asInstanceOf[universe.Type]))
        // obj.symbol.asTerm.typeSignature
        // else
        obj.tpe

      tp
    }

    override def traverse(tree: Tree) = tree match {
      case Apply(ap @ Apply(_, _), args) => // TODO What about type apply?
        val argTypes = args.map(_.tpe)
        parameterLists = argTypes :: parameterLists
        traverse(ap)
        parameterLists = Nil
        for (x <- args) traverse(x)
      case Apply(Select(obj, name), args) =>
        parameterLists = args.map(_.tpe) :: parameterLists
        addIfNotLifted(MethodInv(Some(getObjType(obj)), name.toString, Nil, parameterLists))
        parameterLists = Nil
        for (x <- args) traverse(x)
      case Apply(TypeApply(Select(obj, name), targs), args) =>
        parameterLists = args.map(_.tpe) :: parameterLists
        addIfNotLifted(MethodInv(Some(getObjType(obj)), name.toString, targs, parameterLists))
        parameterLists = Nil
        for (x <- args) traverse(x)
      // TODO think about this feature.
      case tr @ Select(obj, name) => //if isChecked(obj) && tr.symbol.isMethod
        log(s"Select obj.tpe = ${obj.tpe}")
        addIfNotLifted(MethodInv(Some(getObjType(obj)), name.toString, Nil, Nil))
      case tr =>
        log(s"Not Checking: ${showRaw(tr)}")
        super.traverse(tree)
    }

    def analyze(tree: Tree): Boolean = {
      traverse(tree)
      log(s"To analyze: " + (methods ++ lifted))
      //Finds the first element of the sequence satisfying a predicate, if any.
      (methods ++ lifted).toSeq.find(!methodsExist(_)) match {
        case Some(methodError) if lifted.contains(methodError) =>
          // report a language error
          false
        case Some(methodError) =>
          // missing method
          c.error(tree.pos, s"error: Method $methodError not found.")
          false
        case None =>
          true
      }
    }

  }

  case class MethodInv(
    tpe: Option[Type], name: String, targs: List[Tree], args: List[List[Type]])

  def methodsExist(methods: MethodInv*): Boolean = {
    val methodSet = methods.toSet
    def application(meth: MethodInv): Tree = {

      def dummyTree(tpe: Type) = tpe match {
        case typeTag @ ThisType(_) => This(newTypeName(className))
        case _ =>
          log(s"tpe = $tpe typeTree = ${constructTypeTree(tpe)}")
          TypeApply(Select(Literal(Constant(())),
            newTermName("asInstanceOf")), List(constructTypeTree(tpe)))
      }

      def app(tree: Tree) = meth.targs match {
        case Nil => tree
        case tparams: List[Tree] =>
          TypeApply(tree, if (rep)
            meth.targs
          else
            meth.targs map { x => constructPolyTree(x.tpe) })
      }

      meth.tpe.map(dummyTree(_)).orElse(Some(This(className)))
        .map(x => app(Select(x, newTermName(meth.name))))
        .map(f => meth.args.foldLeft(f)((x, y) => Apply(x, y.map(dummyTree)))).get
    }

    val res = try {
      // block containing only dummy methods that were applied.
      val block = Block(composeDSL(Block(methodSet.map(x => application(x)).toSeq: _*)), Literal(Constant(())))
      log("Block before typecheck: " + show(block, printTypes = true))
      log("Block before typecheck: " + showRaw(block))
      c.typeCheck(c.resetAllAttrs(block))
      true
    } catch {
      case e: Throwable =>
        log("Feature not working!!!" + e)
        false
    }

    res
  }

  private final class FreeVariableCollector extends Traverser {

    private[this] val collected = ListBuffer[Symbol]()
    private[this] var defined = List[Symbol]()

    private[this] final def isFree(id: Symbol) = !defined.contains(id)

    override def traverse(tree: Tree) = tree match {
      case i @ Ident(s) => {
        val sym = i.symbol
        if (sym.isTerm && !(sym.isMethod || sym.isPackage || sym.isModule) && isFree(sym)) collected append sym
      }
      case _ => super.traverse(tree)
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
      case vd @ ValDef(mods, name, tpt, rhs) =>
        definedValues += vd.symbol
        traverse(rhs)
      case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        definedMethods += dd.symbol
        vparamss.flatten.foreach(x => traverse(x))
        traverse(rhs)
      case _ =>
        super.traverse(tree)
      //TODO: remove if works correctly
      //      case vd: ValDef => definedValues += vd.symbol
      //      case dd: DefDef => definedMethods += dd.symbol
      //      case _          => super.traverse(tree)
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

  def copy(orig: Tree)(nev: Tree): Tree = {
    nev.setSymbol(orig.symbol)
    nev.setPos(orig.pos)
    nev
  }

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
        case vd @ ValDef(m, n, t, rhs) =>
          copy(vd)(ValDef(m, n, t, Typed(transform(rhs), TypeTree(t.tpe))))

        case dd @ DefDef(m, n, tp, p, rt, rhs) =>
          copy(dd)(DefDef(m, n, tp, p, rt, Typed(transform(rhs), TypeTree(rt.tpe))))

        case ap @ Apply(fun, args) =>
          val ascrArgs = args map {
            x => // TODO cleanup. This can be done easier.
              val auniverse = c.universe.asInstanceOf[scala.reflect.internal.Types]
              log(s"isConstantType(x.tpe) = " +
                auniverse.isConstantType(tree.tpe.asInstanceOf[auniverse.Type]))
              Typed(transform(x), TypeTree(
                if (x.tpe != null &&
                  auniverse.isConstantType(x.tpe.asInstanceOf[auniverse.Type]))
                  x.tpe.erasure
                else
                  x.tpe))
          }
          if (externalApplyFound) {
            Apply(transform(fun), ascrArgs)
          } else {
            externalApplyFound = true
            val baseTree = Apply(transform(fun), ascrArgs)
            externalApplyFound = false
            Typed(baseTree, TypeTree(ap.tpe))
          }
        case _ =>
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

  private final class LiftLiteralTransformer(
    val idents: List[Symbol],
    val free: List[Symbol]) extends Transformer {

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(v)) =>
          Apply(Select(This(newTypeName(className)), newTermName("lift")), List(t))
        case t @ Ident(v) if free.contains(t.symbol) && idents.contains(t.symbol) =>
          Apply(Select(This(newTypeName(className)), newTermName("lift")), List(t))
        case _ =>
          super.transform(tree)
      }
    }

    def apply(tree: Tree) = transform(tree)
  }

  private object VirtualizationTransformer {
    def apply(tree: Tree) = new VirtualizationTransformer()(tree)
  }

  private final class VirtualizationTransformer extends Transformer {

    val lifted = mutable.ArrayBuffer[MethodInv]()
    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ If(cond, then, elze) =>

          // if (!featureExists("__ifThenElse", List(cond.tpe, then.tpe, elze.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the conditionnal branches!")
          // }
          lifted += MethodInv(None, "__ifThenElse", Nil, List(List(cond.tpe, then.tpe, elze.tpe)))
          method("__ifThenElse", List(transform(cond), transform(then), transform(elze)))
        // Variable definition, assignment and return : VariableEmbeddingDSL
        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          // if (!featureExists("__newVar", List(rhs.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the creation of variables!")
          // }
          lifted += MethodInv(None, "__newVar", Nil, List(List(rhs.tpe)))
          ValDef(mods, sym, transform(tpt), method("__newVar", List(transform(rhs)))) // If there is a type transformer, it would be good to transform also the type tree

        case Return(e) =>
          // if (!featureExists("__return", List(e.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the return of values!")
          // }
          lifted += MethodInv(None, "__return", Nil, List(List(e.tpe)))
          method("__return", List(transform(e)))

        case Assign(lhs, rhs) =>
          // if (!featureExists("__assign", List(lhs.tpe, rhs.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the assignment!")
          // }
          lifted += MethodInv(None, "__assign", Nil, List(List(lhs.tpe, rhs.tpe)))
          method("__assign", List(transform(lhs), transform(rhs)))

        // While and DoWhile: ImperativeDSL
        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label), List())), Literal(Constant()))) if label == sym => // While
          // if (!featureExists("__whileDo", List(cond.tpe, body.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the while loops!")
          // }
          lifted += MethodInv(None, "__whileDo", Nil, List(List(cond.tpe, body.tpe)))
          method("__whileDo", List(transform(cond), transform(body)))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label), List()), Literal(Constant())))) if label == sym => // DoWhile
          // if (!featureExists("__doWhile", List(body.tpe, cond.tpe))) {
          // c.error(tree.pos, "The DSL doesn't support the do-while loops!")
          // }
          lifted += MethodInv(None, "__doWhile", Nil, List(List(cond.tpe, body.tpe)))
          method("__doWhile", List(transform(body), transform(cond)))
        case _ =>
          super.transform(tree)
      }
    }

    def apply(tree: Tree) = (transform(tree), lifted.toSeq)
  }

  /**
   * Replace all variables in `toMark` with `hole[](id)`
   */
  private final class HoleMarkerTransformer(toMark: List[Int]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case i @ Ident(s) =>
        val id = symbolId(i)
        symbolIds.put(id, i.symbol)
        if (toMark contains id)
          Apply(
            TypeApply(Select(This(newTypeName(className)), newTermName(holeMethod)), List(TypeTree(), TypeTree())),
            //List(Apply(TypeApply(Select(Select(This(newTypeName("scala")), newTermName("Predef")), newTermName("manifest")), List(TypeTree(i.tpe))), List()),
            List(Apply(TypeApply(Ident(newTermName("manifest")), List(constructPolyTree(i.tpe))), List()),
              Literal(Constant(id))))
        //TODO: remove if it works
        //List(Apply(TypeApply(Ident(newTermName("manifest")), List(TypeTree(i.tpe))), List()),
        //changed because we need to rewire type parameters
        else
          super.transform(tree)
      //case s @ Select(id: Ident, _) if toMark contains id.symbol =>
      //mark(s.symbol)
      case _ =>
        super.transform(tree)
    }

    def mark(tree: Tree): Tree = {
      log(s"To mark: ${toMark.toString}")
      transform(tree)
    }

  }

  object ScopeInjectionTransformer {
    def apply(tree: Tree) = new ScopeInjectionTransformer().transform(VirtualizationTransformer(tree)._1)
  }

  private final class ScopeInjectionTransformer extends Transformer {
    //TODO: remove if works correctly
    //    private val definedValues, definedMethods = collection.mutable.HashSet[Symbol]()

    val notLiftedTypes: Set[Type] = Set(
      c.universe.typeOf[scala.math.Numeric.IntIsIntegral.type],
      c.universe.typeOf[scala.math.Numeric.DoubleIsFractional.type],
      c.universe.typeOf[scala.reflect.ClassTag[Int]].erasure)

    def isLifted(tp: Type): Boolean =
      !(notLiftedTypes exists (_ =:= tp.erasure))

    //TODO: remove if works correctly
    //    /**
    //     * Current solution for finding outer scope idents.
    //     */
    //    def markDSLDefinition(tree: Tree) = tree match {
    //      case _: ValDef => definedValues += tree.symbol
    //      case _: DefDef => definedMethods += tree.symbol
    //      case _         =>
    //    }
    //
    //    private[this] final def isFree(s: Symbol) = !(definedValues.contains(s) || definedMethods.contains(s))

    private[this] final def isHole(tree: Tree): Boolean =
      tree match {
        case Apply(
          TypeApply(Select(This(_), methodName), List(TypeTree())),
          List(Literal(Constant(_: Int)))) if methodName.decoded == holeMethod => true
        case _ => false
      }

    var ident = 0

    override def transform(tree: Tree): Tree = {
      //TODO: remove if works correctly
      //markDSLDefinition(tree)

      log(" " * ident + " ==> " + tree)
      ident += 1

      val result = tree match {
        //provide Def trees with NoSymbol (for correct show(tree)
        case vdDef: ValOrDefDef => {
          val retDef = super.transform(tree)
          retDef.setSymbol(NoSymbol)
          retDef
        }

        case typTree: TypTree if typTree.tpe != null =>
          if (isLifted(typTree.tpe)) {
            constructTypeTree(typTree.tpe)
          } else {
            //TODO move it to tree construction methods (and inject to them)
            constructNotLiftedTree(typTree.tpe)
          }

        // re-wire objects
        case s @ Select(Select(inn, t: TermName), name) // package object goes to this
        if s.symbol.isMethod && rewiredToThis(t.toString) =>
          Ident(name)

        case s @ Select(Select(inn, t: TermName), name) // `this` goes to `this`
        if s.symbol.isMethod && t.toString == "this" =>
          Ident(name)

        case s @ Select(inn, name) if s.symbol.isMethod =>
          Select(transform(inn), name)

        // replaces objects with their cake counterparts
        case s @ Select(inn, name) => // TODO this needs to be narrowed down if s.symbol.isModule =>
          Ident(name)

        //Added to rewire inherited methods to this class
        case th @ This(_) =>
          This(newTypeName(className))

        case TypeApply(mth, targs) => // TODO this needs to be changed for LMS to include a type transformer
          if (rep) { // this check needs to go to the signatures of the method
            //Added because we need to rewire type parameters in TypeApply
            val liftedTargs = targs map { x: Tree => constructPolyTree(x.tpe) }
            TypeApply(transform(mth), liftedTargs)
          } else {
            val liftedTargs = targs map (transform(_))
            TypeApply(transform(mth), liftedTargs)
          }

        // Removes all import statements (for now).
        case Import(_, _) =>
          EmptyTree

        case f @ Function(p, b) =>
          // TODO transform ValDef types here with an explicit type tree.
          Function(p map (x => transform(x).asInstanceOf[ValDef]), transform(b))

        // ignore the hole
        case _ if isHole(tree) =>
          println("***************************************** Does this get invoked? *********************************************")
          tree
        case _ =>
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
  val holeMethod = "hole"
  val className = "generated$" + dslName.filter(_ != '.') + YYTransformer.uID.incrementAndGet
  def constructTypeTree(inType: Type) = if (rep)
    constructRepTree(inType)
  else
    constructPolyTree(inType)
  def isPrimitive(s: Symbol): Boolean = false
  val guardType = "checkRef"

  //TODO (TOASK) - do we need tp.normalize here?
  private def isFunctionType(tp: Type): Boolean = tp.normalize match {
    case TypeRef(pre, sym, args) if args.nonEmpty =>
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
    case _ =>
      false
  }

  def constructNotLiftedTree(inType: Type): Tree = inType match {
    case TypeRef(pre, sym, Nil) =>
      Select(This(newTypeName(className)), inType.typeSymbol.name)

    case TypeRef(pre, sym, args) if isFunctionType(inType) =>
      AppliedTypeTree(
        Select(Ident(newTermName("scala")), sym.name),
        args map { x => TypeTree(x) })

    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(
        Select(This(newTypeName(className)), sym.name),
        args map { x => TypeTree(x) })

    case q @ SingleType(pre, sym) =>
      Select(This(newTypeName(className)), sym.name)

    case another @ _ =>
      TypeTree(another)
  }

  def constructPolyTree(inType: Type): Tree = inType match {

    case TypeRef(pre, sym, Nil) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case TypeRef(pre, sym, Nil) =>
      Select(This(newTypeName(className)), inType.typeSymbol.name)

    case TypeRef(pre, sym, args) if isFunctionType(inType) =>
      AppliedTypeTree(Select(Ident(newTermName("scala")), sym.name),
        args map { x => constructPolyTree(x) })

    case TypeRef(pre, sym, args) =>
      AppliedTypeTree(Select(This(newTypeName(className)), sym.name),
        args map { x => constructPolyTree(x) })

    case ConstantType(t) =>
      Select(This(newTypeName(className)), inType.typeSymbol.name)

    case SingleType(pre, name) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case SingleType(pre, name) if inType.typeSymbol.isModuleClass =>
      SingletonTypeTree(Select(This(newTypeName(className)),
        newTermName(inType.typeSymbol.name.toString)))

    case SingleType(pre, name) if inType.typeSymbol.isClass =>
      Select(This(newTypeName(className)),
        newTermName(inType.typeSymbol.name.toString))

    case another @ _ =>
      println(("!" * 10) + s"""Missed: $inType = ${
        showRaw(another)
      } name = ${inType.typeSymbol.name}""")
      TypeTree(another)
  }

  def constructRepTree(inType: Type): Tree = { //transform Type1[Type2[...]] => Rep[Type1[Type2[...]]] for non-function types
    val universe = c.universe.asInstanceOf[scala.reflect.internal.Types]

    def rep(inType: Type): Tree =
      AppliedTypeTree(Select(This(newTypeName(className)), newTypeName("Rep")),
        List(TypeTree(inType)))

    inType match {
      case inType if isFunctionType(inType) =>
        val TypeRef(pre, sym, args) = inType
        val retTyperees = args map { x => rep(x) }
        //we can't construnct baseTree using TypeTree(pre) - pre is only scala.type not FunctionN
        //val baseTree = TypeTree(pre) //pre = scala.type
        //using such baseTree we get val a: scala.type[generated$dsllarepVectorDSL13.this.Rep[Int], generated$dsllarepVectorDSL13.this.Rep[Int]] = ...
        val baseTree = Select(Ident(newTermName("scala")), sym.name)
        AppliedTypeTree(baseTree, retTyperees)

      case SingleType(pre, name) if inType.typeSymbol.isClass && (!inType.typeSymbol.isModuleClass) =>
        rep(inType)

      case inType if universe.isSingleType(inType.asInstanceOf[universe.Type]) =>
        constructPolyTree(inType)

      case _ =>
        rep(inType)
    }
  }

  /*
   * Utilities.
   */
  private def symbolId(symbol: Symbol): Int =
    symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id

  private def symbolId(tree: Tree): Int = symbolId(tree.symbol)

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
      case head :: tail =>
        Select(tail.foldLeft[Tree](Ident(newTermName(head)))((tree, name) => Select(tree, newTermName(name))), tpeName)
      case Nil =>
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
        DefDef(Modifiers(), newTermName(mainMethod), List(), List(List()), Ident(newTypeName("Any")), transformedBody))))
  //     }
  // }

  def log(s: String) = if (debug) println(s)

}
