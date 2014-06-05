package ch.epfl.lamp
package autolifter
package impl

import scala.language.existentials

import scala.reflect.macros.whitebox.Context
import language.experimental.macros
import ch.epfl.yinyang._
import ch.epfl.yinyang.transformers._
import ch.epfl.yinyang.analysis._
import ch.epfl.yinyang.typetransformers.{ TypeTransformer, RepTransformer }

object ImplLifter {

  def liftClass[T]: Unit = macro implementations.liftClass[T]

  object implementations {

    def liftClass[T](c: Context)(implicit ctag: c.WeakTypeTag[T]): c.Expr[Unit] = {
      import c.universe._

      val liftedMethods = new MacroHelper[c.type](c).liftClass(ctag.tpe)

      liftedMethods foreach {
        case (s, m) =>
          println(s)
          showCode(m)
      }

      val al = new AutoLifter(c.universe)
      val lc = al.getLiftedClass(ctag.tpe)(annotations.Custom())
      val lp = al.getLiftedProgram(lc)
      val liftedMethodsMap = liftedMethods.toMap
      lp.caseClasses foreach { cc =>
        val sym = cc.opsMethod.method.originalSymbol.asInstanceOf[Symbol]
        println(cc + "->" + showCode(liftedMethodsMap(sym)))
      }

      c.Expr[Unit](q"()")
    }
  }
}

class MacroHelper[C <: Context](val c: C) extends MacroModule {
  type Ctx = C
  import c.universe._

  def liftClassModuleSymbol(typeSymbol: c.Symbol, isClass: Boolean, transform: Boolean, dslName: String): List[(Symbol, Tree)] = {
    import c.universe._

    val implLifter = new ImplLifter[c.type](c)(dslName, Map()) {
      val typeTransformer = new RepTransformer[c.type](c)
      typeTransformer.className = dslName
    }

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

    val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)))
    // val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)).map(x => (sourceTree, x)))

    // val imports = classTree.head._1 match {
    //   case PackageDef(_, list) => list collect { case i: Import => i }
    // }

    classTree foreach { x =>
      val file = x.symbol.pos.source.file

      println(s"In file `$file` the class definition had been found!")
    }

    if (classTree.isEmpty) {
      return Nil
    }

    val typedClassTree = c.typecheck(classTree.head.duplicate)
    // val typedClassTree = {
    //   val Block(list, _) = c.typeCheck(c.resetLocalAttrs(Block(imports ++ List(classTree.head._2.duplicate), Literal(Constant(())))))
    //   list.last
    // }

    def getMethods(template: Template): List[(Symbol, Tree)] = template match {
      case Template(parents, self, body) => {
        body collect {
          case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if dd.symbol.isMethod && !dd.symbol.asMethod.isGetter =>
            dd.symbol -> (if (transform) implLifter.transform(Nil)(rhs) else rhs)
        }
      }
    }

    val liftedMethods = typedClassTree match {
      case ClassDef(_, _, _, template) if isClass => getMethods(template)
      case ModuleDef(_, _, template) if !isClass  => getMethods(template)
    }

    liftedMethods
  }

  def liftClassModule(tpe: c.Type, isClass: Boolean, transform: Boolean): List[(Symbol, Tree)] = {
    import c.universe._

    val typeSymbol = tpe.typeSymbol

    val dslName = typeSymbol.name.toString + "Component"
    // val implLifter = new ImplLifter[c.type](c)(dslName, Map()) {
    //   val typeTransformer = new RepTransformer[c.type](c)
    //   typeTransformer.className = dslName
    // }

    // val wholeSource = c.enclosingRun.units.toList map (_.body)

    // def isCorrectClassModuleDef(tree: Tree): Boolean = {
    //   (
    //     tree.symbol != null) &&
    //     (
    //       tree.symbol == { if (isClass) typeSymbol else typeSymbol.companionSymbol }) &&
    //       (
    //         tree match {
    //           case ClassDef(_, _, _, _) => isClass
    //           case ModuleDef(_, _, _)   => !isClass
    //           case _                    => false
    //         })
    // }

    // val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)))
    // // val classTree = wholeSource flatMap (sourceTree => sourceTree.find(tree => isCorrectClassModuleDef(tree)).map(x => (sourceTree, x)))

    // // val imports = classTree.head._1 match {
    // //   case PackageDef(_, list) => list collect { case i: Import => i }
    // // }

    // classTree foreach { x =>
    //   val file = x.symbol.associatedFile
    //   // val file = x._2.symbol.associatedFile
    //   println(s"In file `$file` the class definition had been found!")
    // }

    // val typedClassTree = c.typeCheck(classTree.head.duplicate)
    // // val typedClassTree = {
    // //   val Block(list, _) = c.typeCheck(c.resetLocalAttrs(Block(imports ++ List(classTree.head._2.duplicate), Literal(Constant(())))))
    // //   list.last
    // // }

    // def getMethods(template: Template): List[(Symbol, Tree)] = template match {
    //   case Template(parents, self, body) => {
    //     body collect {
    //       case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if dd.symbol.isMethod && !dd.symbol.asMethod.isGetter =>
    //         dd.symbol -> (if (transform) implLifter.transform(Nil)(rhs) else rhs)
    //     }
    //   }
    // }

    // val liftedMethods = typedClassTree match {
    //   case ClassDef(_, _, _, template) if isClass => getMethods(template)
    //   case ModuleDef(_, _, template) if !isClass  => getMethods(template)
    // }

    val liftedMethods = liftClassModuleSymbol(typeSymbol, isClass, transform, dslName)

    /**
     * for handling inheritance
     */
    val inheritedMethods =
      if (isClass) {
        tpe.baseClasses filter (bc => List(typeOf[Any], typeOf[AnyRef]).map(_.typeSymbol).forall(_ != bc) && bc != typeSymbol) flatMap (bc => liftClassModuleSymbol(bc, isClass, transform, dslName))
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

  val scopeTransforming: Boolean = false

  val constructorConverting: Boolean = false

  // Members declared in ch.epfl.yinyang.TransformationUtils
  def debugLevel: Int = 2

  def transform(toLift: List[Symbol])(block: Tree): Tree =
    (
      ((x: Tree) => VirtualizationTransformer(x)._1) andThen
      AscriptionTransformer andThen
      LiftLiteralTransformer(toLift, Nil) andThen
      {
        if (scopeTransforming)
          ScopeInjectionTransformer(_)
        else
          identity[Tree](_)
      } andThen
      TypeTreeTransformer andThen
      {
        if (constructorConverting)
          ConstructorConvertor
        else
          identity[Tree]
      } /* andThen
      composeDSL(toLift)*/ )(
        block)

  def apply[T](block: c.Expr[T]): c.Expr[T] = {
    val dslTree = transform(Nil)(block.tree)
    // println(dslTree)
    c.Expr[T](c.untypecheck(dslTree))
  }

  object ConstructorConvertor extends Transformer with (Tree => Tree) {
    def apply(tree: Tree): Tree = transform(tree)

    override def transform(tree: Tree): Tree = {
      tree match {
        case Select(New(Ident(name)), sel) => {
          Select(Ident(TermName(name.toString + "New")), TermName("apply"))
        }
        case _ => super.transform(tree)
      }
    }

  }

}