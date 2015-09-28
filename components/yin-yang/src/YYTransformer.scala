package ch.epfl
package yinyang

import ch.epfl.yinyang.api._
import ch.epfl.yinyang.transformers._
import ch.epfl.yinyang.analysis._
import scala.collection.immutable.Map
import scala.reflect.macros.blackbox.Context
import scala.collection.mutable
import mutable.{ ListBuffer, HashMap }
import language.experimental.macros
import java.util.concurrent.atomic.AtomicLong
import yinyang.typetransformers.TypeTransformer

object YYTransformer {
  val defaults = Map[String, Any](
    ("shallow" -> true),
    ("debug" -> 0),
    ("shortenDSLNames" -> true),
    ("mainMethod" -> "main"),
    ("featureAnalysing" -> true),
    ("virtualizeFunctions" -> false),
    ("failCompilation" -> true),
    ("ascriptionTransforming" -> true),
    ("liftTypes" -> Nil),
    ("optionalInitiallyStable" -> true),
    ("codeCacheSize" -> 3),
    ("minimumCountToStabilize" -> 500))

  def apply[C <: Context, T](c: C)(
    dslName: String,
    tpeTransformer: TypeTransformer[c.type],
    postProcessing: Option[PostProcessing[c.type]],
    preProcessing: Option[PreProcessing[c.type]],
    config: Map[String, Any] = Map()): YYTransformer[c.type, T] =

    new YYTransformer[c.type, T](c, dslName, config withDefault (defaults)) {
      val typeTransformer = tpeTransformer
      typeTransformer.className = className
      val postProcessor = postProcessing.getOrElse(new NullPostProcessing[c.type](c))
      val preProcessor = preProcessing.getOrElse(new NullPreProcessing[c.type](c))
    }

  protected[yinyang] val uID = new AtomicLong(0)
}

// for now configuration goes as a named parameter list
abstract class YYTransformer[C <: Context, T](val c: C, dslName: String, val config: Map[String, Any])
  extends LanguageVirtualization
  with ScopeInjection
  with HoleTransformation
  with FreeIdentAnalysis
  with AscriptionTransformation
  with TypeTreeTransformation
  with LiftLiteralTransformation
  with DataDefs
  with TransformationUtils
  with YYConfig {

  type Ctx = C
  import c.universe._
  val postProcessor: PostProcessing[c.type]
  val preProcessor: PreProcessing[c.type]
  import typeTransformer._
  import postProcessor._
  import preProcessor._

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
    log("-------- YYTransformer STARTED for block: " + showRaw(block.tree), 2)

    def shallowFlag = shallow || (c.settings contains ("embed"))
    if (featureAnalysing) // Aborts compilation for unsupported constructs
      FeatureAnalyzer(block.tree)

    if (shallowFlag) { block }
    else {
      // mark captured variables as holes
      val captured: List[Tree] = freeVariables(block.tree)
      val capturedSyms: List[Symbol] = captured.map(_.symbol)
      log("Free variables (capturedSyms): " + capturedSyms, 2)
      log("original: " + block, 2)

      /*
       * Transforms the given DSL program block.
       *   @param toHoles All contained symbolIds will be replaced with
       *     `hole[T](classTag[T], holeId)` and the holeTable will map from
       *     holeId to symbolId
       *   @param toMixed All contained symbolIds will be replaced with
       *     `lift("captured$" + sym, hole[T](classTag[T], holeId))` and the
       *     holeTable will map from holeId to symbolId
       *   @param toLifts All contained symbols will be replaced with
       *     `lift("captured$" + sym)`
       */
      def transform(toHoles: List[Symbol], toMixed: List[Symbol], toLifts: List[Symbol])(block: Tree): Tree =
        ((injectImport _) andThen
          PreProcess andThen
          AscriptionTransformer andThen
          (x => VirtualizationTransformer(x)._1) andThen
          LiftLiteralTransformer(toLifts, toMixed) andThen
          TypeTreeTransformer andThen
          ScopeInjectionTransformer andThen
          HoleTransformer((toHoles ++ toMixed).distinct, className) andThen
          composeDSL((toLifts ++ toMixed).distinct) andThen
          PostProcess)(block)

      lazy val unboundDSL = {
        val t = transform(capturedSyms, Nil, Nil)(block.tree)
        log("FIRST TRANSFORM DONE (prettyPrinting in Caps):\n" + code(t) + "\n", 2)
        log(show(t, printTypes = true), 3)
        t
      }
      val varTypes: List[(Tree, VarType)] = dslType match {
        case tpe if tpe <:< typeOf[FullyStaged] =>
          captured.map(t => (t, defaultCompVar(t.symbol)))

        case tpe if tpe <:< typeOf[FullyUnstaged] =>
          captured.map(t => (t, NonCompVar()))

        case tpe if tpe <:< typeOf[HoleTypeAnalyser] => {
          capturedSyms.foreach { s => log(s.typeSignature.typeConstructor.toString, 3) }
          captured.map(t => (t,
            if (liftTypes.exists(l => t.symbol.typeSignature <:< l.asInstanceOf[Type]))
              defaultCompVar(t.symbol)
            else NonCompVar()))
        }

        case tpe =>
          captured.zip(
            reflInstance[BaseYinYang](unboundDSL)
              .compilationVars(capturedSyms.asInstanceOf[List[reflect.runtime.universe.Symbol]]))
      }

      val compilVars: List[Tree] = varTypes.collect { case (s, _: CompVar) => s }
      val nonCompilVars: List[Tree] = captured diff compilVars
      val guards: List[Guard] = varTypes.collect { case (_, v: CompVar) => v.guard }
      val liftedCompilVars: List[Tree] = varTypes.collect { case (s, _: RequiredStaticCompVar) => s }
      val mixedCompilVars: List[Tree] = compilVars diff liftedCompilVars

      log(s"VarTypes: $varTypes", 2)
      log(s"capturedSyms: $capturedSyms\nnonCompilVars (holes): $nonCompilVars" +
        s"compilVars: $compilVars\nlifted: $liftedCompilVars\nmixed: $mixedCompilVars", 2)

      // re-transform the tree with new holes if there are required vars
      val dsl = if (compilVars.isEmpty) unboundDSL
      else {
        holeTable.clear()
        transform(
          nonCompilVars.map(_.symbol), mixedCompilVars.map(_.symbol), liftedCompilVars.map(_.symbol))(
            block.tree)
      }

      // if the DSL inherits the StaticallyChecked trait reflectively do the static analysis
      if (dslType <:< typeOf[StaticallyChecked]) {
        log("statically checking", 2)
        reflInstance[StaticallyChecked](dsl).staticallyCheck(new Reporter(c))
      }

      val sortedHoles = (nonCompilVars ++ mixedCompilVars)
        .map(x => (x.symbol, x)).toMap.values.toSeq
        .sortBy(h => holeTable.indexOf(symbolId(h)))

      def args(holes: List[Symbol]): List[Symbol] = holes
      // holes.map({ y: Symbol => y.name.decodedName.toString })

      val programId = new scala.util.Random().nextLong
      val programIdTree = Literal(Constant(programId))
      val dslTree = dslType match {
        case tpe if tpe <:< typeOf[CodeGenerator] && compilVars.isEmpty && !(tpe <:< typeOf[FullyStaged]) =>
          // does not require run-time data => completely generated at compile.
          log("COMPILE TIME COMPILED", 2)

          q"""
            ch.epfl.yinyang.runtime.YYStorage.incrementCompileTimeCompileCount(${programIdTree})
            ${c parse (reflInstance[CodeGenerator](dsl) generateCode className)}
            new ${Ident(TypeName(className))}().apply(..${captured})
          """
        case tpe if tpe <:< typeOf[Stager] && compilVars.isEmpty =>
          log("COMPILE TIME COMPILED for lifting", 2)
          val retType = deepDealias(block.tree.tpe)
          q"""
            $dsl
            val dslInstance = new ${Ident(TypeName(className))}()
            import dslInstance._
            dslInstance.stage[dslInstance.Rep[$retType]]()
          """
        case _ =>
          /*
           * Requires run-time variables => execute at run-time and install a recompilation guard.
           */
          log("RUNTIME COMPILED with guards", 2)
          val retType = deepDealias(block.tree.tpe)
          val retTypeString = retType.toString
          val functionTypeString =
            s"""${sortedHoles.map(_ => "scala.Any").mkString("(", ", ", ")")} => ${retTypeString}"""
          val functionType = tq"(..${sortedHoles.map(_ => tq"scala.Any")}) => ${retType}"

          log("Guard function strings: " + guards, 3)

          val optionalHoleIds = varTypes.collect({
            case (s, _: Optional) => holeTable.indexOf(symbolId(s))
            case (_, _: CompVar)  => -1
          }) mkString ("scala.Array((", "), (", "))")

          val YYCacheString = YYStorageFactory.getYYStorageString(
            className, functionTypeString, retTypeString,
            guards, optionalHoleIds, optionalInitiallyStable, codeCacheSize, minimumCountToStabilize,
            compilVars.map(_.symbol.asInstanceOf[reflect.runtime.universe.Symbol]))

          val guardedExecute = dslType match {
            case t if t <:< typeOf[CodeGenerator] =>
              q"""
                val cache = ${c parse YYCacheString}
                val compilVars: scala.Array[Any] = scala.Array(..$compilVars)
                val program = ch.epfl.yinyang.runtime.YYStorage.guardedLookup[${functionType}](
                  ${programId}, cache, compilVars)
                program.apply(..${sortedHoles})
              """
            // TODO(vsalvis) How do optional variables interact with interpretation?
            // FIXME: this is not tested! Types probably wrong
            // case t if t <:< typeOf[Interpreted] => q"""
            //   val dslInstance = ${c parse YYCacheString}
            //   val compilVars: scala.Array[Any] = scala.Array(..$compilVars)
            //   ${compilVars.map({ k => "dslInstance.captured$" + k.symbol.toString + " = " + k.symbol.toString }) mkString "\n"}
            //   def invalidate(): () => Any = () => dslInstance.reset
            //   dslInstance.check(compilVars, invalidate)
            //   dslInstance.interpret[${retTypeString}](..${sortedHoles})
            // """
          }
          Block(List(dsl), guardedExecute)
      }

      log(s"Final tree: ${showRaw(c.untypecheck(dslTree))}", 3)
      log(s"Final untyped: ${show(c.untypecheck(dslTree), printTypes = true)}", 3)
      log(s"Final typed: ${show(c.typecheck(c.untypecheck(dslTree)), printTypes = true)}\n")
      log("-------- YYTransformer DONE ----------\n\n", 2)
      c.Expr[T](c.untypecheck(dslTree))
    }
  }

  /*
  * Configuration parameters.
  */
  def interpretMethod = "interpret"
  val holeMethod = "hole"
  val classUID = YYTransformer.uID.incrementAndGet
  override val className =
    s"generated$$${dslName.filter(_ != '.') + classUID}"
  val dslType = c.mirror.staticClass(dslName).toType
  def debugLevel: Int = debug

  /*
   * Aborts with a compilation error if any features used are restricted in the deep embedding.
   */
  object FeatureAnalyzer extends ((Tree, Seq[DSLFeature]) => Unit) {
    def apply(tree: Tree, lifted: Seq[DSLFeature] = Seq()): Unit = {
      log("Feature analysis", 2)
      val (virtualized, lifted) = virtualize(tree)
      val st = System.currentTimeMillis()
      new FeatureAnalyzer(lifted).analyze(virtualized)
      log(s"Feature checking time: ${(System.currentTimeMillis() - st)}\n", 3)
    }
  }

  private class FeatureAnalyzer(val lifted: Seq[DSLFeature]) extends Traverser {

    var methods = mutable.LinkedHashSet[DSLFeature]()

    var parameterLists: List[List[Type]] = Nil

    def addIfNotLifted(m: DSLFeature) =
      if (!lifted.exists(x => x.name == m.name)) {
        log(s"adding ${m}", 3)
        methods += m
      }

    def getObjType(obj: Tree): Type = obj.tpe

    override def traverse(tree: Tree) = tree match {
      case Apply(ap @ Apply(_, _), args) =>
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
        log(s"Select obj.tpe = ${obj.tpe}", 3)
        addIfNotLifted(DSLFeature(Some(getObjType(obj)), name.toString, Nil, Nil))
      case tr =>
        log(s"Not Checking: ${showRaw(tr)}", 3)
        super.traverse(tree)
    }

    // Aborts compilation with a compilation error if any features are missing.
    def analyze(tree: Tree): Unit = {
      traverse(tree)
      log(s"To analyze: " + (methods ++ lifted), 2)
      //Finds the first element of the sequence satisfying a predicate, if any.
      (methods ++ lifted).toSeq.find(!methodsExist(_)) match {
        case Some(methodError) if lifted.contains(methodError) =>
          val name = methodError.name.replaceAll("infix_", "").replaceAll("__ifThenElse", "if")
          c.error(tree.pos, s"Language construct ${name} not supported.")
        case Some(DSLFeature(tpe, name, _, _)) =>
          // missing method
          val tpName = tpe
            .map(x => x.toString.replaceAll("\\.type", ""))
            .map(_ + ".").getOrElse("")
          c.error(tree.pos, s"Operation $tpName$name not supported by the DSL $dslName.")
        case None =>
      }
    }
  }

  def methodsExist(methods: DSLFeature*): Boolean = {
    val methodSet = methods.toSet
    def application(meth: DSLFeature): Tree = {

      def dummyTree(tpe: Type) = tpe match {
        case typeTag @ ThisType(_) =>
          This(TypeName(className))
        case _ =>
          q"().asInstanceOf[${constructTypeTree(typeTransformer.OtherCtx, tpe)}]"
      }

      log(s"Args: ${meth.args}", 3)
      val lhs = typeApply(meth.targs map { x => constructTypeTree(typeTransformer.TypeArgCtx, x.tpe) })(
        Select(meth.tpe.map(dummyTree(_)).getOrElse(This(TypeName(className))), TermName(meth.name)))
      val res = meth.args.foldLeft(lhs)((x, y) => Apply(x, y.map(dummyTree)))
      log(s"${showRaw(res)}", 3)
      res
    }

    val res = try {
      // block containing only dummy methods that were applied.
      val block = Block(List(
        composeDSL(Nil)(Block(
          methodSet.map(x => application(x)).toList,
          Literal(Constant(()))))), Literal(Constant(())))
      log(s"Block: ${show(block)})", 3)
      log(s"Block raw: ${showRaw(block)})", 3)
      c.typecheck(block)
      true
    } catch {
      case e: Throwable =>
        log("Feature not working!!!" + e, 2)
        false
    }

    res
  }

  def defaultCompVar(s: Symbol): CompVar = {
    CompVar.equality(s.typeSignature.toString)
  }

  private lazy val dslTrait = {
    val names = dslName.split("\\.").toList.reverse
    assert(names.length >= 1,
      s"DSL trait name must be in the valid format. DSL trait name is ${dslName}")

    val tpeName = TypeName(names.head)
    names.tail.reverse match {
      case head :: tail =>
        Select(tail.foldLeft[Tree](Ident(TermName(head)))((tree, name) =>
          Select(tree, TermName(name))), tpeName)
      case Nil =>
        Ident(tpeName)
    }
  }

  private var _reflInstance: Option[Object] = None

  /**
   * Reflectively instantiate and memoize a DSL instance.
   */
  private def reflInstance[T](dslDef: Tree): T = {
    if (_reflInstance == None) {
      log("Reflectively instantiating and memoizing DSL.", 2)
      val st = System.currentTimeMillis()
      _reflInstance = Some(c.eval(
        c.Expr(c.untypecheck(Block(List(dslDef), q"new ${Ident(TypeName(className))}()")))))
      log(s"Eval time: ${(System.currentTimeMillis() - st)}", 2)
    } else {
      log("Retrieving memoized reflective DSL instance.", 2)
    }
    _reflInstance.get.asInstanceOf[T]
  }

  def composeDSL(compilVars: List[Symbol])(transformedBody: Tree): Tree = q"""
    class ${TypeName(className)} extends $dslTrait {
      ..${compilVars.map(k => q"var ${TermName("captured$" + k.name.decodedName.toString)} = $k")}
      def main(): Any = {$transformedBody}
    }
  """

  def injectImport(body: Tree): Tree = {
    val typed = c.typecheck(q"""import _root_.ch.epfl.yinyang.runtime._;
      ${c.untypecheck(body)}
    """)
    val Block(List(_), virtualizedBody) = typed
    virtualizedBody
  }
}
