package ch.epfl.lamp.yinyang

import ch.epfl.lamp.yinyang.api._
import scala.collection.mutable
import mutable.{ ListBuffer, HashMap }
import scala.collection.immutable.Map
import scala.reflect.macros.Context
import language.experimental.macros
import scala.reflect.runtime.universe.definitions.FunctionClass
import java.util.concurrent.atomic.AtomicLong

object YYTransformer {

  def apply[C <: Context, T](c: C, dslName: String,
                             shallow: Boolean = true,
                             debug: Boolean = false,
                             rep: Boolean = false,
                             refCheck: Boolean = false,
                             slickHack: Boolean = false) =
    new YYTransformer(c, dslName, shallow, debug, rep, slickHack)

  protected[yinyang] val uID = new AtomicLong(0)
}

// for now configuration goes as a named parameter list
final class YYTransformer[C <: Context, T](
  val c: C,
  dslName: String,
  val shallow: Boolean = false,
  val debug: Boolean = false,
  val rep: Boolean = false,
  val slickHack: Boolean = false,
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
    log("Body: " + showRaw(block.tree))
    // shallow or detect a non-existing feature => return the original block.
    if (!FeatureAnalyzer(block.tree) || shallow)
      block
    else {
      // mark captured variables as holes
      val allCaptured = freeVariables(block.tree)
      def transform(holes: List[Int], idents: List[Symbol])(block: Tree): Tree =
        (AscriptionTransformer andThen
          LiftLiteralTransformer(idents) andThen
          ScopeInjectionTransformer andThen
          HoleTransformer(holes) andThen
          composeDSL)(block)

      val dslPre = transform(allCaptured map symbolId, Nil)(block.tree)

      log(s"Pre Eval: ${show(dslPre)}")
      // if the DSL inherits the StaticallyChecked trait reflectively do the static analysis
      if (reflInstance(dslPre).isInstanceOf[StaticallyChecked])
        reflInstance(dslPre).asInstanceOf[StaticallyChecked].staticallyCheck(c)

      // DSL returns what holes it needs
      val reqVars =
        reflInstance(dslPre).asInstanceOf[BaseYinYang].stagingAnalyze(allCaptured map symbolId)
          .map(symbolById)
      val holes = allCaptured diff reqVars

      // re-transform the tree with new holes if there are required vars
      val dsl = if (reqVars.isEmpty) dslPre
      else transform(holes map symbolId, reqVars)(block.tree)

      def args(holes: List[Symbol]): String =
        holes.map({ y: Symbol => y.name.decoded }).mkString("", ",", "")

      val dslTree = reflInstance(dsl) match {
        case dslInst: CodeGenerator if reqVars.isEmpty =>
          /*
           * If DSL does not require run-time data it can be completely
           * generated at compile time and wired for execution.
           */
          c parse s"""
            ${dslInst generateCode className}
            new $className().apply(${args(allCaptured)})
          """
        case dslInst =>
          /*
           * If DSL need run-time info send it to run-time and install recompilation guard.
           */
          val programId = new scala.util.Random().nextLong
          val retType = block.tree.tpe.toString
          val functionType = s"""${(0 until args(holes).length).map(y => "scala.Any").mkString("(", ", ", ")")} => ${retType}"""
          val refs = reqVars filterNot (isPrimitive)
          val dslInit = s"""
            val dslInstance = ch.epfl.lamp.yinyang.runtime.YYStorage.lookup(${programId}L, new $className())
            val values: Seq[Any] = Seq(${(reqVars diff refs) map (_.name.decoded) mkString ", "})
            val refs: Seq[Any] = Seq(${refs map (_.name.decoded) mkString ", "})
          """

          val guardedExecute = c parse dslInit + (dslInst match {
            case _: CodeGenerator => s"""
              def recompile(): () => Any = dslInstance.compile[$retType, $functionType]
              val program = ch.epfl.lamp.yinyang.runtime.YYStorage.$guardType[$functionType](
                ${programId}L, values, refs, recompile
              )
              program.apply(${args(holes)})
            """
            case _: Interpreted => s"""
              def invalidate(): () => Any = () => dslInstance.reset
              ch.epfl.lamp.yinyang.runtime.YYStorage.$guardType[Any](
                ${programId}L, values, refs, invalidate
              )
              dslInstance.interpret[${retType}](${args(holes)})
            """
          })

          Block(dsl, guardedExecute)
      }

      log(s"""Final untyped: ${show(c.resetAllAttrs(dslTree))}
        Final typed: ${show(c.typeCheck(c.resetAllAttrs(dslTree)))}""")
      c.Expr[T](c.resetAllAttrs(dslTree))
    }
  }

  def rewiredToThis(s: String) = s == "package" || s == "Predef"
  def freeVariables(tree: Tree): List[Symbol] =
    new FreeVariableCollector().collect(tree)

  object FeatureAnalyzer extends ((Tree, Seq[DSLFeature]) => Boolean) {
    def apply(tree: Tree, lifted: Seq[DSLFeature] = Seq()): Boolean = {
      /*val (virtualized, lifted) = VirtualizationTransformer(tree)
      val st = System.currentTimeMillis()
      val res = new FeatureAnalyzer(lifted).analyze(virtualized)
      log(s"Feature checking time: ${(System.currentTimeMillis() - st)}")
      res*/
      true
    }

  }

  private final class FeatureAnalyzer(val lifted: Seq[DSLFeature]) extends Traverser {

    var methods = mutable.LinkedHashSet[DSLFeature]()

    var parameterLists: List[List[Type]] = Nil

    def addIfNotLifted(m: DSLFeature) =
      if (!lifted.exists(x => x.name == m.name)) {
        log(s"adding ${m}")
        methods += m
      }

    def getObjType(obj: Tree): Type = obj.tpe

    override def traverse(tree: Tree) = tree match {
      case Apply(ap @ Apply(_, _), args) => // TODO What about type apply?
        val argTypes = args.map(_.tpe)
        parameterLists = argTypes :: parameterLists
        traverse(ap)
        parameterLists = Nil
        for (x <- args) traverse(x)
      case Apply(Select(obj, name), args) =>
        parameterLists = args.map(_.tpe) :: parameterLists
        addIfNotLifted(DSLFeature(Some(getObjType(obj)), name.toString, Nil, parameterLists))
        parameterLists = Nil
        for (x <- args) traverse(x)
      case Apply(TypeApply(Select(obj, name), targs), args) =>
        parameterLists = args.map(_.tpe) :: parameterLists
        addIfNotLifted(DSLFeature(Some(getObjType(obj)), name.toString, targs, parameterLists))
        parameterLists = Nil
        for (x <- args) traverse(x)
      case tr @ Select(obj, name) =>
        log(s"Select obj.tpe = ${obj.tpe}")
        addIfNotLifted(DSLFeature(Some(getObjType(obj)), name.toString, Nil, Nil))
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
          c.error(tree.pos, s"Language feature not supported.")
          false
        case Some(methodError) =>
          // missing method
          c.error(tree.pos, s"Method $methodError not found.")
          false
        case None =>
          true
      }
    }

  }

  case class DSLFeature(
    tpe: Option[Type], name: String, targs: List[Tree], args: List[List[Type]])

  def methodsExist(methods: DSLFeature*): Boolean = {
    val methodSet = methods.toSet
    def application(meth: DSLFeature): Tree = {

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
          TypeApply(tree, meth.targs map { x => constructPolyTree(x.tpe) })
      }
      log(s"Args ${meth.args}")
      val res = meth.tpe.map(dummyTree(_)).orElse(Some(This(className)))
        .map(x => app(Select(x, newTermName(meth.name))))
        .map(f => meth.args.foldLeft(f)((x, y) => Apply(x, y.map(dummyTree)))).get
      log(s"${showRaw(res)}")
      res
    }

    val res = try {
      // block containing only dummy methods that were applied.
      val block = Block(composeDSL(Block(
        methodSet.map(x => application(x)).toSeq: _*)), Literal(Constant(())))
      log(s"Block: ${show(block)})")
      log(s"Block raw: ${showRaw(block)})")
      c.typeCheck(block)
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
        //store info about idents
        symbolIds.put(symbolId(sym), sym)
        if (sym.isTerm &&
          !(sym.isMethod || sym.isPackage || sym.isModule) &&
          isFree(sym)) collected append sym
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
        vparamss.flatten.foreach(traverse)
        traverse(rhs)
      case _ =>
        super.traverse(tree)
    }

    def definedSymbols(tree: Tree): List[Symbol] = {
      definedValues.clear()
      definedMethods.clear()
      traverse(tree)
      (definedValues ++ definedMethods).toList
    }

  }

  def constructor(classname: String, arguments: List[Tree]): Tree =
    invoke(newClass(classname), nme.CONSTRUCTOR, arguments)

  def copy(orig: Tree)(nev: Tree): Tree = {
    nev.setSymbol(orig.symbol)
    nev.setPos(orig.pos)
    nev
  }

  object AscriptionTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = tree //new AscriptionTransformer().transform(tree)
  }

  private final class AscriptionTransformer extends Transformer {
    var ident = 0
    var externalApplyFound = false

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
    def apply(idents: List[Symbol] = Nil)(tree: Tree) =
      new LiftLiteralTransformer(idents).transform(tree)
  }

  private final class LiftLiteralTransformer(val idents: List[Symbol])
    extends Transformer {

    def lift(t: Tree) =
      Apply(Select(This(newTypeName(className)), newTermName("lift")), List(t))

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) =>
          lift(t)
        case t @ Ident(_) if idents.contains(t.symbol) =>
          lift(t)
        case _ =>
          super.transform(tree)
      }
    }
  }

  private object VirtualizationTransformer {
    def apply(tree: Tree) = new VirtualizationTransformer()(tree)
  }

  private final class VirtualizationTransformer extends Transformer {

    val lifted = mutable.ArrayBuffer[DSLFeature]()
    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ If(cond, then, elze) =>
          lifted += DSLFeature(None, "__ifThenElse", Nil, List(List(cond.tpe, then.tpe, elze.tpe)))
          method("__ifThenElse", List(transform(cond), transform(then), transform(elze)))

        case ValDef(mods, sym, tpt, rhs) if mods.hasFlag(Flag.MUTABLE) =>
          lifted += DSLFeature(None, "__newVar", Nil, List(List(rhs.tpe)))
          ValDef(mods, sym, transform(tpt), method("__newVar", List(transform(rhs))))

        case Return(e) =>
          lifted += DSLFeature(None, "__return", Nil, List(List(e.tpe)))
          method("__return", List(transform(e)))

        case Assign(lhs, rhs) =>
          lifted += DSLFeature(None, "__assign", Nil, List(List(lhs.tpe, rhs.tpe)))
          method("__assign", List(transform(lhs), transform(rhs)))

        case LabelDef(sym, List(), If(cond, Block(body :: Nil, Apply(Ident(label),
          List())), Literal(Constant()))) if label == sym => // While
          lifted += DSLFeature(None, "__whileDo", Nil, List(List(cond.tpe, body.tpe)))
          method("__whileDo", List(transform(cond), transform(body)))

        case LabelDef(sym, List(), Block(body :: Nil, If(cond, Apply(Ident(label),
          List()), Literal(Constant())))) if label == sym => // DoWhile
          lifted += DSLFeature(None, "__doWhile", Nil, List(List(cond.tpe, body.tpe)))
          method("__doWhile", List(transform(body), transform(cond)))
        case _ =>
          super.transform(tree)
      }
    }

    def apply(tree: Tree) = (transform(tree), lifted.toSeq)
  }

  object HoleTransformer {
    def apply(toMark: List[Int] = Nil)(tree: Tree) =
      new HoleTransformer(toMark).transform(tree)
  }
  /**
   * Replace all variables in `toMark` with `hole[T](classTag[T], symbolId)`
   */
  private final class HoleTransformer(toMark: List[Int]) extends Transformer {

    override def transform(tree: Tree): Tree = tree match {
      case i @ Ident(s) if toMark contains symbolId(i.symbol) =>
        Apply(
          Select(This(newTypeName(className)), newTermName(holeMethod)),
          List(
            TypeApply(Ident(newTermName("manifest")), List(TypeTree(i.tpe))),
            Literal(Constant(symbolId(i.symbol)))))
      case _ =>
        super.transform(tree)
    }
  }

  object ScopeInjectionTransformer extends (Tree => Tree) {
    def apply(tree: Tree) =
      new ScopeInjectionTransformer().transform(VirtualizationTransformer(tree)._1)
  }

  private final class ScopeInjectionTransformer extends Transformer {

    private[this] final def isHole(tree: Tree): Boolean =
      tree match {
        case Apply(TypeApply(Select(This(_), methodName), List(TypeTree())),
          List(Literal(Constant(_: Int)))) if methodName.decoded == holeMethod => true
        case _ => false
      }

    var ident = 0

    override def transform(tree: Tree): Tree = {
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
          constructTypeTree(typTree.tpe)

        // re-wire objects
        case s @ Select(Select(inn, t: TermName), name) // package object goes to this
        if s.symbol.isMethod && (rewiredToThis(t.toString) || t.toString == "this") =>
          Ident(name)

        case s @ Select(inn, name) if s.symbol.isMethod =>
          Select(transform(inn), name)

        // replaces objects with their cake counterparts
        case s @ Select(inn, name) =>
          Ident(name)

        //Added to rewire inherited methods to this class
        case th @ This(_) =>
          This(newTypeName(className))

        case TypeApply(mth, targs) =>
          if (rep) { // this check needs to go to the signatures of the method
            //Added because we need to rewire type parameters in TypeApply
            val liftedTargs = targs map { x: Tree => constructPolyTree(x.tpe) }
            TypeApply(transform(mth), liftedTargs)
          } else {
            val liftedTargs =
              if (slickHack)
                targs
              else
                targs map (transform(_))
            TypeApply(transform(mth), liftedTargs)
          }

        // Removes all import statements (for now).
        case Import(_, _) =>
          EmptyTree

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
  val MaxFunctionArity = 22
  def interpretMethod = "interpret"
  val holeMethod = "hole"
  val className =
    s"generated$$${dslName.filter(_ != '.') + YYTransformer.uID.incrementAndGet}"
  def constructTypeTree(inType: Type) = if (rep)
    constructRepTree(inType)
  else
    constructPolyTree(inType)
  def isPrimitive(s: Symbol): Boolean = false
  val guardType = "checkRef"

  private def isFunctionType(tp: Type): Boolean = tp.normalize match {
    case TypeRef(pre, sym, args) if args.nonEmpty =>
      val arity = args.length - 1

      arity <= MaxFunctionArity &&
        arity >= 0 &&
        sym.fullName == FunctionClass(arity).fullName
    case _ =>
      false
  }

  /* Do we really need this */
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

  def toType(s: Symbol) = s.name
  def constructPolyTree(inType: Type): Tree = inType match {

    case TypeRef(pre, sym, Nil) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case TypeRef(pre, sym, Nil) =>
      Select(This(newTypeName(className)), toType(inType.typeSymbol))

    case TypeRef(pre, sym, args) if isFunctionType(inType) =>
      AppliedTypeTree(Select(Ident(newTermName("scala")), toType(sym)),
        args map { x => constructPolyTree(x) })

    case TypeRef(pre, sym, args) => {
      val liftedArgs =
        if (slickHack)
          args map { x => TypeTree(x) }
        else
          args map { x => constructPolyTree(x) }
      AppliedTypeTree(Select(This(newTypeName(className)), toType(sym)),
        liftedArgs)
    }

    case ConstantType(t) =>
      Select(This(newTypeName(className)), toType(inType.typeSymbol))

    case SingleType(pre, name) if rewiredToThis(inType.typeSymbol.name.toString) =>
      SingletonTypeTree(This(tpnme.EMPTY))

    case SingleType(pre, name) if inType.typeSymbol.isModuleClass =>
      SingletonTypeTree(Select(This(newTypeName(className)),
        newTermName(inType.typeSymbol.name.toString)))

    case s @ SingleType(pre, name) if inType.typeSymbol.isClass =>
      constructPolyTree(
        s.asInstanceOf[scala.reflect.internal.Types#SingleType]
          .underlying.asInstanceOf[YYTransformer.this.c.universe.Type])

    case another @ _ =>
      println(("!" * 10) + s"""Missed: $inType = ${
        showRaw(another)
      } name = ${inType.typeSymbol.name}""")
      TypeTree(another)
  }

  /*
   * transform Type1[Type2[...]] => Rep[Type1[Type2[...]]] for non-function types
   */
  def constructRepTree(inType: Type): Tree = {
    val universe = c.universe.asInstanceOf[scala.reflect.internal.Types]

    def rep(inType: Type): Tree = {
      log(s"${"!" * 10} Rep ${showRaw(inType)}")
      AppliedTypeTree(Select(This(newTypeName(className)), newTypeName("Rep")),
        List(constructPolyTree(inType))) // TypeTree(inType)
    }

    inType match {
      case inType if isFunctionType(inType) =>
        val TypeRef(pre, sym, args) = inType
        val retTyperees = args map { x => rep(x) }
        //we can't construnct baseTree using TypeTree(pre) - pre is only scala.type not FunctionN
        //val baseTree = TypeTree(pre) //pre = scala.type
        //using such baseTree we get val a: scala.type[Rep[Int], Rep[Int]] = ...
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
    assert(names.length >= 1,
      s"DSL trait name must be in the valid format. DSL trait name is ${dslName}")

    val tpeName = newTypeName(names.head)
    names.tail.reverse match {
      case head :: tail =>
        Select(tail.foldLeft[Tree](Ident(newTermName(head)))((tree, name) =>
          Select(tree, newTermName(name))), tpeName)
      case Nil =>
        Ident(tpeName)
    }
  }

  private var _reflInstance: Option[Object] = None

  /**
   * Reflectively instantiate and memoize a DSL instance.
   */
  private def reflInstance(dslDef: Tree) = {
    if (_reflInstance == None) {
      val st = System.currentTimeMillis()
      _reflInstance = Some(c.eval(
        c.Expr(c.resetAllAttrs(Block(dslDef, constructor(className, List()))))))
      log(s"Eval time: ${(System.currentTimeMillis() - st)}")
    }
    _reflInstance.get
  }

  private def constructor = Apply(Select(New(Ident(newTypeName(className))),
    nme.CONSTRUCTOR), List())

  def composeDSL(transformedBody: Tree) =
    // class MyDSL extends DSL {
    ClassDef(Modifiers(), newTypeName(className), List(),
      Template(List(dslTrait), emptyValDef,
        List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY),
              nme.CONSTRUCTOR), List())), Literal(Constant(())))),
          // def main = {
          DefDef(Modifiers(), newTermName(mainMethod), List(), List(List()),
            Ident(newTypeName("Any")), transformedBody))))
  //     }
  // }

  def log(s: => String) = if (debug) println(s)

}

object Debug {

  def show[T](x: => T): T = macro showImpl[T]
  def showImpl[T](c: Context)(x: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    println(s"Tree ${showRaw(x)}")
    x
  }
}