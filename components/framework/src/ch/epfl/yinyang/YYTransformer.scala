package ch.epfl
package yinyang

import ch.epfl.yinyang.api._
import ch.epfl.yinyang.transformers._
import ch.epfl.yinyang.analysis._
import scala.collection.immutable.Map
import scala.reflect.macros.Context
import scala.collection.mutable
import mutable.{ ListBuffer, HashMap }
import language.experimental.macros
import java.util.concurrent.atomic.AtomicLong
import yinyang.typetransformers.TypeTransformer

object YYTransformer {
  val defaults = Map[String, Any](
    ("shallow" -> true),
    ("debug" -> 0),
    ("mainMethod" -> "main"),
    ("featureAnalysing" -> true),
    ("ascriptionTransforming" -> true))

  def apply[C <: Context, T](c: C)(
    dslName: String,
    tpeTransformer: TypeTransformer[c.type],
    postProcessing: Option[PostProcessing[c.type]],
    config: Map[String, Any] = Map()) =
    new YYTransformer[c.type, T](c, dslName, config withDefault (defaults)) {
      val typeTransformer = tpeTransformer
      typeTransformer.className = className
      // val postProcessor = postProcessing
      // val postProcessor = postProcessing.get
      val postProcessor = postProcessing.getOrElse(new NullPostProcessing[c.type](c))
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
  with LiftLiteralTransformation
  with DataDefs
  with TransformationUtils
  with YYConfig {

  type Ctx = C
  import c.universe._
  val typeTransformer: TypeTransformer[c.type]
  val postProcessor: PostProcessing[c.type]
  import typeTransformer._
  import postProcessor._

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
    if (featureAnalysing && (!FeatureAnalyzer(block.tree) || shallow))
      block
    else {
      // mark captured variables as holes
      val allCaptured = freeVariables(block.tree)
      def transform(holes: List[Int], idents: List[Symbol])(block: Tree): Tree =
        (AscriptionTransformer andThen
          LiftLiteralTransformer(idents) andThen
          (x => VirtualizationTransformer(x)._1) andThen
          ScopeInjectionTransformer andThen
          TypeTreeTransformer andThen
          HoleTransformer(holes) andThen
          composeDSL andThen
          PostProcess)(block)

      val dslPre = transform(allCaptured map symbolId, Nil)(block.tree)

      // if the DSL inherits the StaticallyChecked trait reflectively do the static analysis
      if (dslType <:< typeOf[StaticallyChecked])
        reflInstance[StaticallyChecked](dslPre).staticallyCheck(new Reporter(c))

      log(showRaw(dslPre, printTypes = true))
      // c.typeCheck()
      // DSL returns what holes it needs
      val reqVars = dslType match {
        case tpe if tpe <:< typeOf[FullyStaged] =>
          allCaptured
        case tpe =>
          reflInstance[BaseYinYang](dslPre).requiredHoles.map(symbolById)
      }

      val holes = allCaptured diff reqVars

      // re-transform the tree with new holes if there are required vars
      val dsl = if (reqVars.isEmpty) dslPre
      else transform(holes map symbolId, reqVars)(block.tree)

      def args(holes: List[Symbol]): String =
        holes.map({ y: Symbol => y.name.decoded }).mkString("", ",", "")

      val dslTree = dslType match {
        case tpe if tpe <:< typeOf[CodeGenerator] && reqVars.isEmpty =>
          /*
           * If DSL does not require run-time data it can be completely
           * generated at compile time and wired for execution.
           */
          c parse s"""
            ${reflInstance[CodeGenerator](dsl) generateCode className}
            new $className().apply(${args(allCaptured)})
          """
        case _ =>
          /*
           * If DSL need run-time info send it to run-time and install recompilation guard.
           */
          val programId = new scala.util.Random().nextLong
          val retType = block.tree.tpe.toString
          val functionType = s"""${(0 until args(holes).length).map(y => "scala.Any").mkString("(", ", ", ")")} => ${retType}"""
          val refs = reqVars filterNot (isPrimitive)
          val dslInit = s"""
            val dslInstance = ch.epfl.yinyang.runtime.YYStorage.lookup(${programId}L, new $className())
            val values: Seq[Any] = Seq(${(reqVars diff refs) map (_.name.decoded) mkString ", "})
            val refs: Seq[Any] = Seq(${refs map (_.name.decoded) mkString ", "})
          """

          val guardedExecute = c parse dslInit + (dslType match {
            case t if t <:< typeOf[CodeGenerator] => s"""
              def recompile(): () => Any = dslInstance.compile[$retType, $functionType]
              val program = ch.epfl.yinyang.runtime.YYStorage.$guardType[$functionType](
                ${programId}L, values, refs, recompile
              )
              program.apply(${args(holes)})
            """
            case t if t <:< typeOf[Interpreted] => s"""
              def invalidate(): () => Any = () => dslInstance.reset
              ch.epfl.yinyang.runtime.YYStorage.$guardType[Any](
                ${programId}L, values, refs, invalidate
              )
              dslInstance.interpret[${retType}](${args(holes)})
            """
          })

          Block(dsl, guardedExecute)
      }

      log(s"""Final untyped: ${show(c.resetAllAttrs(dslTree), printTypes = true)}
        Final typed: ${show(c.typeCheck(c.resetAllAttrs(dslTree)))}""")
      c.Expr[T](c.resetAllAttrs(dslTree))
    }
  }

  /*
  * Configuration parameters.
  */
  def interpretMethod = "interpret"
  val holeMethod = "hole"
  val className =
    s"generated$$${dslName.filter(_ != '.') + YYTransformer.uID.incrementAndGet}"
  val dslType = c.mirror.staticClass(dslName).toType
  def debugLevel: Int = debug

  /*
   * Checking if the all functionality from the shallow embedding exists.
   */
  object FeatureAnalyzer extends ((Tree, Seq[DSLFeature]) => Boolean) {
    def apply(tree: Tree, lifted: Seq[DSLFeature] = Seq()): Boolean = {
      val (virtualized, lifted) = virtualize(tree)
      val st = System.currentTimeMillis()
      val res = new FeatureAnalyzer(lifted).analyze(virtualized)
      log(s"Feature checking time: ${(System.currentTimeMillis() - st)}")
      res
    }

  }

  private class FeatureAnalyzer(val lifted: Seq[DSLFeature]) extends Traverser {

    var methods = mutable.LinkedHashSet[DSLFeature]()

    var parameterLists: List[List[Type]] = Nil

    def addIfNotLifted(m: DSLFeature) =
      if (!lifted.exists(x => x.name == m.name)) {
        log(s"adding ${m}")
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
          c.error(tree.pos, s"Language feature $methodError not supported.")
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

  def methodsExist(methods: DSLFeature*): Boolean = {
    val methodSet = methods.toSet
    def application(meth: DSLFeature): Tree = {

      def dummyTree(tpe: Type) = tpe match {
        case typeTag @ ThisType(_) =>
          This(newTypeName(className))
        case _ =>
          TypeApply(
            Select(Literal(Constant(())), newTermName("asInstanceOf")),
            List(constructTypeTree(typeTransformer.OtherCtx, tpe)))
      }

      log(s"Args ${meth.args}")
      val lhs: Tree = typeApply(meth.targs map { x => constructTypeTree(typeTransformer.TypeApplyCtx, x.tpe) })(
        Select(meth.tpe.map(dummyTree(_)).getOrElse(This(className)), newTermName(meth.name)))
      val res = meth.args.foldLeft(lhs)((x, y) => Apply(x, y.map(dummyTree)))
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

  object TypeTreeTransformer extends (Tree => Tree) {
    def apply(tree: Tree) = new TypeTreeTransformer().transform(tree)
  }

  private final class TypeTreeTransformer extends Transformer {

    var typeCtx: TypeContext = OtherCtx

    override def transform(tree: Tree): Tree = {
      val result = tree match {
        case typTree: TypTree if typTree.tpe != null =>
          constructTypeTree(typeCtx, typTree.tpe)

        case TypeApply(mth, targs) =>
          // TypeApply params need special treatment
          typeCtx = TypeApplyCtx
          val liftedArgs = targs map (transform(_))
          typeCtx = OtherCtx
          TypeApply(transform(mth), liftedArgs)

        case _ =>
          super.transform(tree)
      }

      result
    }
  }

  def constructTypeTree(tctx: TypeContext, inType: Type): Tree =
    typeTransformer transform (tctx, inType)

  def isPrimitive(s: Symbol): Boolean = false
  val guardType = "checkRef"

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
  private def reflInstance[T](dslDef: Tree): T = {
    if (_reflInstance == None) {
      val st = System.currentTimeMillis()
      _reflInstance = Some(c.eval(
        c.Expr(c.resetAllAttrs(Block(dslDef, constructor)))))
      log(s"Eval time: ${(System.currentTimeMillis() - st)}")
    }
    _reflInstance.get.asInstanceOf[T]
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

}