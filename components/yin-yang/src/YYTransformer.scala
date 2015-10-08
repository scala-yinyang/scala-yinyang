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
    ("direct" -> true),
    ("debug" -> 0),
    ("shortenDSLNames" -> true),
    ("mainMethodName" -> "main"),
    ("restrictLanguage" -> true),
    ("virtualizeFunctions" -> false),
    ("failCompilation" -> true),
    ("ascribeTerms" -> true),
    ("liftTypes" -> Nil))
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

    def directFlag = direct || (c.settings contains ("embed"))
    if (restrictLanguage) // Aborts compilation for unsupported constructs
      FeatureAnalyzer(block.tree)

    if (directFlag) { block }
    else {
      // mark captured variables as holes
      val captured: List[Tree] = freeVariables(block.tree)
      val capturedSyms: List[Symbol] = captured.map(_.symbol)

      /*
       * Transforms the given DSL program block.
       *   @param toHoles All contained symbolIds will be replaced with
       *     `hole[T](classTag[T], holeId)` and the holeTable will map from
       *     holeId to symbolId
       *   @param toLifts All contained symbols will be replaced with
       *     `lift("captured$" + sym)`
       */
      def transform(toHoles: List[Symbol], toLifts: List[Symbol])(block: Tree): Tree =
        ((injectImport _) andThen
          (c.untypecheck(_)) andThen (c.typecheck(_)) andThen // virtualizes pattern matching
          PreProcess andThen
          AscriptionTransformer andThen
          (x => VirtualizationTransformer(x)._1) andThen
          LiftLiteralTransformer(toLifts) andThen
          HoleTransformer(toHoles.distinct, className) andThen
          (c.untypecheck(_)) andThen (c.typecheck(_)) andThen // returns the lost type information
          (removeImport _) andThen // removes the shallow imports
          TypeTreeTransformer andThen
          ScopeInjectionTransformer andThen
          (composeDSL _) andThen
          PostProcess)(block)

      lazy val dsl = transform(capturedSyms, Nil)(block.tree)

      // if the DSL inherits the StaticallyChecked trait reflectively do the static analysis
      if (dslType <:< typeOf[StaticallyChecked]) {
        log("statically checking", 2)
        reflInstance[StaticallyChecked](dsl).staticallyCheck(new Reporter(c))
      }

      val sortedHoles = captured.map(x => (x.symbol, x)).toMap.values.toSeq
        .sortBy(h => holeTable.indexOf(symbolId(h)))

      val dslTree = dslType match {
        case tpe if tpe <:< typeOf[CodeGenerator] && !(tpe <:< typeOf[Staged]) =>
          // does not require run-time data => completely generated at compile.
          log("COMPILE TIME COMPILED", 2)

          q"""
            ${c parse (reflInstance[CodeGenerator](dsl) generateCode className)}
            new ${Ident(TypeName(className))}().apply(..${sortedHoles})
          """

        case tpe if tpe <:< typeOf[Reified] =>
          log("RUNTIME COMPILED for LIFTING", 2)
          val retType = deepDealias(block.tree.tpe)
          q"""
            $dsl
            val dslInstance = new ${Ident(TypeName(className))}
            dslInstance.reify(dslInstance.$$tpe[$retType])
          """

        case tpe if tpe <:< typeOf[Executable] =>
          /*
           * Requires run-time variables => execute at run-time and install a recompilation guard.
           */
          log("RUNTIME COMPILED and EXECUTED", 2)
          val retType = deepDealias(block.tree.tpe)

          q"""
            $dsl
            val dslInstance = new ${Ident(TypeName(className))}
            dslInstance.execute[$retType](..${sortedHoles})
          """

        case _ => c.abort(c.enclosingPosition, "DSL does not extend adequate traits!")
      }

      log(s"Final tree: ${showRaw(c.untypecheck(dslTree))}", 3)
      log(s"Final untyped: ${show(c.untypecheck(dslTree), printTypes = true)}", 3)
      log(s"Final typed: ${show(c.typecheck(c.untypecheck(dslTree)), printTypes = true)}\n", 3)
      log("-------- YYTransformer DONE ----------\n\n", 2)
      c.Expr[T](c.untypecheck(dslTree))
    }
  }

  /*
  * Configuration parameters.
  */
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
        composeDSL(Block(
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

  def composeDSL(transformedBody: Tree): Tree = q"""
    class ${TypeName(className)} extends $dslTrait {
      def main(): Any = {$transformedBody}
    }
  """

  def injectImport(body: Tree): Tree = q"""
    import _root_.ch.epfl.yinyang.shallow._;
    ${body}
  """

  def removeImport(body: Tree): Tree = {
    val Block(List(_), virtualizedBody) = body

    // TODO: Get a better name! It is not really only removing imports.
    object DropPackageTransformer extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case q"ch.epfl.yinyang.shallow.`package`.$$lift[${ _ }]" => q"$$lift"
          case q"ch.epfl.yinyang.shallow.`package`.$$hole[${ _ }]" => q"$$hole"
          case q"ch.epfl.yinyang.shallow.`package`.$x" => Ident(x)
          case Ident(x) => Ident(x)
          case _ => super.transform(tree)
        }
      }
    }

    DropPackageTransformer.transform(virtualizedBody)
  }
}
