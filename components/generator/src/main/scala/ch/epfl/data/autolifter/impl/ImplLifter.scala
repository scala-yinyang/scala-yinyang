package ch.epfl.data
package autolifter
package impl

import scala.language.existentials

import scala.reflect.macros.whitebox.Context
import language.experimental.macros
import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import ch.epfl.yinyang.analysis._
import ch.epfl.yinyang.typetransformers.{ TypeTransformer, PardisRepTransformer }
import types._

object ImplLifter {
}

class MacroHelper[C <: Context](val c: C) extends MacroModule {
  type Ctx = C
  import c.universe._

  def liftClassModuleSymbol(typeSymbol: c.Symbol, isClass: Boolean, transform: Boolean, dslName: String): List[(Symbol, Tree)] = {
    import c.universe._

    val implLifter = new ImplLifter[c.type](c)(dslName, Map()) {
      val typeTransformer = new PardisRepTransformer[c.type](c)
      typeTransformer.className = dslName
    }

    // println("=== lifting " + typeSymbol + " ===")

    val wholeSource = c.enclosingRun.units.toList map (_.body)

    def isCorrectClassModuleDef(tree: Tree): Boolean = {
      (
        tree.symbol != null) &&
        (
          tree.symbol == { if (isClass) typeSymbol else typeSymbol.companionSymbol }) &&
          (
            tree match {
              case ClassDef(_, _, _, _) => isClass
              case ModuleDef(_, _, _)   => !isClass
              case _                    => false
            })
    }

    // val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)))
    val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)).map(x => (sourceTree, x)))

    // println(s"classTree--: $classTree")

    val imports = classTree.head._1 match {
      case PackageDef(_, list) => list collect { case i: Import => i }
    }

    classTree foreach { x =>
      val file = x._2.symbol.pos.source.file
      // val file = x.symbol.pos.source.file

      println(s"In file `$file` the class definition had been found!")
    }

    if (classTree.isEmpty) {
      return Nil
    }

    // val typedClassTree = c.typecheck(classTree.head.duplicate)
    val typedClassTree = {
      val Block(list, _) = c.typecheck(c.untypecheck(Block(imports ++ List(classTree.head._2.duplicate), Literal(Constant(())))))
      // println("class lists: " + list)
      list.head
    }

    def getMethods(template: Template): List[(Symbol, Tree)] = template match {
      case Template(parents, self, body) => {
        body collect {
          case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if dd.symbol.isMethod && !dd.symbol.asMethod.isGetter =>
            dd.symbol -> (if (transform) implLifter(rhs) else rhs)
        }
      }
    }

    // println(s"typedClassTree--: $typedClassTree")

    val liftedMethods = typedClassTree match {
      case ClassDef(_, _, _, template) if isClass => getMethods(template)
      case ModuleDef(_, _, template) if !isClass  => getMethods(template)
    }

    // println(s"liftedMethods--: $liftedMethods")

    liftedMethods
  }

  def liftClassModule(tpe: c.Type, isClass: Boolean, transform: Boolean): List[(Symbol, Tree)] = {
    import c.universe._

    val typeSymbol = tpe.typeSymbol

    val dslName = typeSymbol.name.toString + "Component"

    val liftedMethods = liftClassModuleSymbol(typeSymbol, isClass, transform, dslName)

    val shouldInherit = true

    /**
     * for handling inheritance
     */
    val inheritedMethods =
      if (shouldInherit && isClass) {
        tpe.baseClasses filter (bc => List(typeOf[Any], typeOf[AnyRef], typeOf[Serializable], typeOf[java.io.Serializable], typeOf[scala.Product], typeOf[scala.Equals]).map(_.typeSymbol).forall(_ != bc) && bc != typeSymbol) flatMap (bc => liftClassModuleSymbol(bc, isClass, transform, dslName))
      } else {
        Nil
      }

    liftedMethods ++ inheritedMethods
  }

  def liftClass(tpe: c.Type, transform: Boolean = true): List[(Symbol, Tree)] = liftClassModule(tpe, true, transform)

  def liftModule(tpe: c.Type, transform: Boolean = true): List[(Symbol, Tree)] = liftClassModule(tpe, false, transform)
}

abstract class ImplLifter[C <: Context](val c: C)(override val className: String, val config: Map[String, Any])
  extends MacroModule
  with TransformationUtils
  with LanguageVirtualization
  with ScopeInjection
  with FreeIdentAnalysis
  with TypeTreeTransformation
  with AscriptionTransformation
  with LiftLiteralTransformation {

  val typeTransformer: TypeTransformer[c.type]
  type Ctx = C
  import c.universe._
  import typeTransformer._

  // Members declared in ch.epfl.yinyang.transformers.AscriptionTransformation
  def ascriptionTransforming: Boolean = false

  val scopeTransforming: Boolean = true

  val constructorConverting: Boolean = true

  // Members declared in ch.epfl.yinyang.TransformationUtils
  def debugLevel: Int = 0

  def transform(toLift: List[Symbol])(block: Tree): Tree =
    (
      ((x: Tree) => VirtualizationTransformer(x)._1) andThen
      AscriptionTransformer andThen
      LiftLiteralTransformer(toLift, Nil) andThen
      {
        if (constructorConverting)
          ConstructorConvertor
        else
          identity[Tree]
      } andThen
      {
        if (scopeTransforming)
          ScopeInjectionTransformer(_)
        else
          identity[Tree](_)
      } andThen
      TypeTreeTransformer /* andThen
      composeDSL(toLift)*/ )(
        block)

  def apply(block: Tree): Tree = {
    // println("/*before trans\n" + showRaw(block) + "\n*/")
    val dslTree = transform(Nil)(block)
    // println(dslTree)
    // c.untypecheck(dslTree)
    dslTree
  }

  object ConstructorConvertor extends Transformer with (Tree => Tree) {
    def apply(tree: Tree): Tree = transform(tree)

    def generateConstructor(className: String) = Select(Ident(TermName(className + "New")), TermName("apply"))

    override def transform(tree: Tree): Tree = {
      tree match {
        case Select(New(Ident(name)), sel) => {
          generateConstructor(name.toString)
        }
        case Select(New(t: TypeTree), sel) => {
          t.original match {
            case Ident(name) => generateConstructor(name.toString)
            case _ => {
              println("constructor found with origin but w/o possiblity to lift:" + showRaw(t.original))
              super.transform(tree)
            }
          }
        }
        case _ => super.transform(tree)
      }
    }

  }

}

abstract class PreProcessor[C <: Context](val c: C)(classTpe: Type, typeContext: TypeContext) extends MacroModule {
  type Ctx = C
  import c.universe._
  var isModule: Boolean = false

  object PreProcess extends Transformer with (Tree => Tree) {
    def apply(tree: Tree): Tree = transform(tree)

    override def transform(tree: Tree): Tree = {
      tree match {
        case Select(This(typeNames.EMPTY), nme @ TermName(name)) =>
          if (isModule) Ident(name) else q"lowering(self).$nme"
        case _ => super.transform(tree)
      }
    }
  }
}

abstract class PostProcessor[C <: Context](val c: C)(classTpe: Type, typeContext: TypeContext) extends MacroModule {
  type Ctx = C
  import c.universe._
  var isModule: Boolean = false

  val typeClasses = List("Manifest", "Numeric", "Fractional", "Ordering", "scala.reflect.AnyValManifest", "scala.reflect.Manifest", "ClassTag", "scala.reflect.ClassTag") ++ typeContext.tcsFullName

  object PostProcess extends Transformer with (Tree => Tree) {
    def apply(tree: Tree): Tree = transform(tree)

    def isLibrary(tree: Tree): Boolean = {
      def s[T: TypeTag]: Symbol = implicitly[TypeTag[T]].tpe.termSymbol
      // import ch.epfl.lamp.autolifter.library._
      val librarySymbols = Nil
      (tree.symbol != null) && (librarySymbols.contains(tree.symbol) || tree.toString == "scala.this.Predef" || tree.toString == "java.this.lang" || tree.toString == "ch.epfl.lamp.autolifter.library" || tree.toString.endsWith("this.`package`") || tree.toString == "ch.epfl.lamp.autolifter.library.`package`")
    }

    def isLibraryType(tpe: Type): Boolean = {
      def t[T: TypeTag]: Type = implicitly[TypeTag[T]].tpe.typeConstructor
      // import ch.epfl.lamp.autolifter.library._
      val libraryTypes = List[Type]()

      val res = (libraryTypes.exists(t => t =:= tpe.typeConstructor)) || typeContext.types.exists(t => (tpe.typeSymbol.name.decoded) equals t.name) || typeContext.tcsFullName.exists(t => (tpe.typeSymbol.fullName) equals t)
      // println(tpe.toString + "->" + tpe.typeSymbol.name.decoded + "==" + res + " -- " + (typeContext.typeClasses.head + "," + tpe.typeSymbol.fullName))
      res
    }

    def isImplicitType(tpe: Type): Boolean = {
      val str = tpe.toString
      // println(tpe)
      typeClasses.exists(tc => str.startsWith(tc))
    }

    def isImplicitApplication(module: Tree): Boolean = {
      val implicitModules = List("scala.math.Numeric.Implicits", "scala.math.Ordering.Implicits", "ch.epfl.lamp.autolifter.library.`package`", "scala.this.Predef") ++ typeContext.tcsModuleName
      implicitModules.exists(im => im == module.toString) || module.toString.endsWith("this.`package`")
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case t: TypeTree if {
          t.tpe != null &&
            // println(t.tpe.typeConstructor.toString + "==>" + (t.tpe.typeConstructor =:= typeOf[ch.epfl.lamp.autolifter.library.ForgeArray[_]].typeConstructor) + ", " + (t.tpe.typeConstructor =:= originalType.asInstanceOf[Type].typeConstructor))
            isLibraryType(t.tpe)
        } => {
          val name = t.symbol.name.toString
          val inType = t.tpe

          // must be pushed to the YinYang
          def tpeConvert(inType: Type): Tree = inType match {
            case TypeRef(pre, sym, Nil) =>
              Ident(inType.typeSymbol.name)
            case TypeRef(pre, sym, args) =>
              AppliedTypeTree(Ident((sym).name),
                args map { x => tpeConvert(x) })
            case ConstantType(t) =>
              TypeTree(inType)
            case _ =>
              TypeTree(t.tpe.normalize)
          }

          tpeConvert(inType)
          // TypeTree.setType(t.tpe.normalize)
        }
        // case Select(This(typeName), sel) if (typeName.toString == classTpe.name)  => if (isModule) Ident(sel) else Select(Ident(newTermName("self")), sel)
        // case This(typeName) if (typeName.toString == classTpe.name)               => if (isModule) This(tpnme.EMPTY) else Ident("self")
        case Select(This(typeName), sel) if typeContext.types.exists(_.name == typeName.toString) => if (isModule) Ident(sel) else Select(Ident(newTermName("self")), sel)
        // case Select(New(Ident(typeName)), sel) if { println(s"typeName $typeName"); typeContext.types.exists(_.name == typeName.toString) } => Select(Ident(TermName("new" + typeName.toString)), TermName("apply"))
        case This(typeName) if typeContext.types.exists(_.name == typeName.toString) => if (isModule) This(tpnme.EMPTY) else Ident(newTermName("self"))
        case Select(x, lift) if lift.toString == "lift" => Ident(newTermName("unit"))
        case This(typeName) if (typeName.toString == classTpe.name + "Component") => Ident(newTermName("this"))
        case Select(module, sel) if isLibrary(module) => Ident(sel)
        case selTree @ Select(module, sel) if typeContext.types.exists(_.name == module.toString) => {
          val sym = selTree.symbol
          if (sym != null && sym.isMethod) {
            val msym = sym.asMethod
            // compiler methods should be changed to this version
            if (sym.isProtected) {
              Ident(sel)
            } else {
              if (sel.toString == "apply")
                selTree
              else {
                selTree
              }
            }
          } else {
            selTree
          }
          // if (sel.toString == "apply") Ident(module.symbol.name) else Ident(sel)
        }
        case Apply(TypeApply(sel @ Select(module, method), _), List(app)) if isImplicitApplication(module) && sel.symbol.asTerm.isImplicit => transform(app)
        case Apply(sel @ Select(module, method), List(app)) if isImplicitApplication(module) && sel.symbol.asTerm.isImplicit               => transform(app)
        case Apply(app, params) if ({ params exists (p => p.tpe != null && isImplicitType(p.tpe)) } || {

          app match {
            case TypeApply(sel, List(tparam)) if sel.toString == "implicitly" || sel.toString == "scala.this.Predef.implicitly" => true
            case _ => false

          }
        }) =>
          transform(app)
        // HACK for Tuple2.apply[T1, T2](v1, v2) -> Tuple2.apply[Rep[T1], Rep[T2]](v1, v2) by changing to Tuple2.apply(v1, v2)
        // case Apply(TypeApply(sel @ Select(module, method), tpes), apps) if module.symbol.name.toString == "Tuple2" && method.toString == "apply" => Apply(sel, apps map transform)
        // HACK for range.foreach[U](func) -> range.foreach(func)
        // case Apply(TypeApply(sel @ Select(module, method), tpes), List(func)) if module.tpe.typeSymbol.name.toString == "Range" && method.toString == "foreach" => Apply(transform(sel), List(transform(func)))
        case _ => super.transform(tree)
      }
    }

  }
}