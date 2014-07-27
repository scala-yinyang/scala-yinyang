package ch.epfl.lamp
package autolifter

import scala.collection.mutable.ArrayBuffer
import annotations.{ Custom, CMethod }
import types._
import scala.reflect.api.Universe

/**
 * This file enables auto-lifting classes via reflection.
 */
class AutoLifter(override val universe: Universe) extends Generator with UniverseContainer {

  def autoLift[T: universe.TypeTag]: String =
    autoLift(Custom())

  def autoLift[T](config: Custom)(implicit ttag: universe.TypeTag[T]): String = {
    autoLiftType(config)(ttag.tpe)
  }

  def autoLiftType(config: Custom)(tpe: Universe#Type): String = {
    val lc = getLiftedClass(tpe)(config)
    val lm = getLiftedModule(tpe)(config)
    val prog = getLiftedProgram(lc, lm)
    generate(prog)
  }

  // def isImplicitSymbol(s: Universe#Symbol): Boolean = s.isSynthetic && s.isImplicit
  def isImplicitSymbol(s: Universe#Symbol): Boolean = s.isImplicit

  def checkMethodOwner(tpe: Universe#Type, m: Universe#MethodSymbol)(implicit config: Custom): Boolean = {
    config.parentLevel match {
      case 0 => m.owner == tpe.typeSymbol
      case 1 => List(universe.typeOf[Any].typeSymbol, universe.typeOf[AnyRef].typeSymbol).forall(_ != m.owner)
      case _ => true
    }
  }

  def methodShouldBeLifted(tpe: Universe#Type, m: Universe#MethodSymbol)(implicit config: Custom): Boolean =
    (checkMethodOwner(tpe, m) && !m.isGetter && !m.isSetter /*&& !(m.name.toString equals "toString")*/ && methodIsInList(m) && !m.isSynthetic) || (
      !config.methods.isEmpty && methodIsInList(m))

  def methodIsInList(m: Universe#MethodSymbol)(implicit config: Custom): Boolean = config.methods match {
    case Nil  => true
    case list => list.exists(x => x.name == m.name.decoded) && methodSpecifications(m)
  }

  def methodConfig(m: Universe#MethodSymbol)(implicit config: Custom): Option[CMethod] = {
    val methods = config.methods.filter(_.name == m.name.decoded)
    // println(s"$m methods: $methods")
    methods match {
      case Nil                                   => None
      case l @ List(x) if l.head.paramss.isEmpty => Some(x)
      case l => {
        val res = l filter (x => { x.paramss map (_.map(_.tpe.toString)) } equals (m.paramss map (_.map(_.typeSignature.toString))))
        // println(s"$m: $res")
        res.headOption
      }
    }
  }

  def methodSpecifications(m: Universe#MethodSymbol)(implicit config: Custom): Boolean = {
    val NO_VAR_ARGS = true
    def isVarArg(tpe: Universe#Type) = tpe.typeSymbol.name.decoded == "<repeated>"
    val varArg = if (NO_VAR_ARGS) {
      !(
        m.paramss exists (l =>
          l exists (p =>
            isVarArg(p.typeSignature))))
    } else {
      true
    }
    val correctOverload =
      if (config.methods.isEmpty)
        true
      else {
        !methodConfig(m).isEmpty
      }
    varArg && correctOverload
  }

  def getMethods(tpe: Universe#Type)(implicit config: Custom): List[LiftedMethod] = {
    val methods = new ArrayBuffer[LiftedMethod]

    tpe.members.toList.reverse.filter((m: Universe#Symbol) => m.isMethod && methodShouldBeLifted(tpe, m.asMethod)) foreach { m =>

      // println(m.name)
      val mt0 = m.asTerm.typeSignature
      val isPoly = mt0.takesTypeArgs

      methods += LiftedMethod(m.asMethod, methodConfig(m.asMethod))

    }

    for (i <- 0 until methods.length) {
      val m1 = methods(i)
      var overloadCount: Int = 1
      if (m1.overloadOrder.isEmpty) {
        for (j <- i + 1 until methods.length) {
          val m2 = methods(j)
          if (m1.symbol.name equals m2.symbol.name) {
            if (overloadCount == 1) {
              m1.overloadOrder = Some(1)
            }
            overloadCount += 1
            m2.overloadOrder = Some(overloadCount)
          }
        }
      }
    }

    methods.toList
  }

  def getLiftedClass(tpe: Universe#Type)(implicit config: Custom): LiftedClass = {
    val implicitParameters = (tpe.members filter (m => isImplicitSymbol(m))).toList
    val typeParams = if (tpe.typeSymbol.typeSignature.takesTypeArgs)
      tpe.typeSymbol.typeSignature.asInstanceOf[Universe#PolyType].typeParams map (_.asType)
    else
      Nil

    val methods = getMethods(tpe)
    // tpe.members doesn't keep the order of fields, whereas declarations keep it
    val fields = tpe.declarations /*members*/ filter ((m: Universe#Symbol) => m.isTerm && {
      val v = m.asTerm
      v.isVar || (v.isVal && v.isGetter)
    }) map (_.asTerm)
    val clazz = LiftedClass(tpe, methods, fields.toList, typeParams, implicitParameters)

    clazz
  }

  def getLiftedModule(tpe: Universe#Type)(implicit config: Custom): LiftedModule = {
    val methods = getMethods(tpe.typeSymbol.companionSymbol.typeSignature)
    val noCons = methods.filter(x => !x.symbol.isConstructor)
    val module = LiftedModule(tpe, noCons.toList)
    module
  }

  def liftedMethodToMethod(lc: LiftedClass, method: LiftedMethod): Method = {
    val name =
      if (method.symbol.isConstructor)
        lc.tpe.typeSymbol.name.toString
      else
        method.symbol.name.toString
    val allParams = method.symbol.paramss.flatten
    val typeParams = (method.symbol.typeParams map (Type.apply _))
    val existingImplicitParams = allParams filter isImplicitSymbol map Parameter.apply
    // duplication of Manifest[T] is removed in the next line
    val implicitParams = (typeParams map (_.manifest) filter (man => existingImplicitParams.forall(eip => eip.tpe != man.tpe))) ++ (existingImplicitParams) ++
      (
        method.overloadOrder map { i =>
          Parameter(Variable(s"overload$i"), new Type(s"Overloaded$i", Nil))
        }).toList
    val paramss = method.symbol.paramss filter (ps => ps exists (p => !isImplicitSymbol(p))) map (l => l map (p => Parameter.apply(p)))
    val returnTpe = Type(method.symbol.returnType)(universe)
    val effect = method.effect
    Method(name, typeParams, paramss, returnTpe, implicitParams, effect, method.symbol)
  }

  def getLiftedProgram(lc: LiftedClass, lm: LiftedModule): LiftedProgram = {

    val selfParam = Parameter(Variable("self"), Type(lc.tpe.typeSymbol))
    val implParams = (lc.typeParams map (Type.apply) map (_.manifest)) ++ (lc.implicitParams map Parameter.apply)

    def repMethod(method: LiftedMethod): RepMethod = {
      val m = liftedMethodToMethod(lc, method)
      RepMethod(m.lift)
    }

    def opsMethod(method: LiftedMethod, rm: RepMethod): OpsMethod = {
      val clzName = lc.tpe.typeSymbol.name.toString
      val prefName = clzName.head.toLower + clzName.tail
      val name = if (rm != null)
        prefName + method.symbol.name.toString.capitalize + method.overloadOrder.getOrElse("")
      // "_" + lc.tpe.typeSymbol.name.toString + method.symbol.name.toString.capitalize + method.overloadOrder.getOrElse("")
      else
        prefName + "New"
      // "_" + lc.tpe.typeSymbol.name.toString + "New"
      val allParams = method.symbol.paramss.flatten
      val typeParams = ((lc.typeParams) ++ (method.symbol.typeParams map (_.asType))) map (Type.apply _)
      val implicitParams = (typeParams map (_.manifest)) ++ (
        ({ if (rm == null) Nil else lc.implicitParams } ++ (allParams filter isImplicitSymbol)) map (p => Parameter(p)))
      val params = {
        // println(s"allParams: $allParams")
        // allParams foreach {p => println(p.isImplicit)}
        val ps = (allParams filter (p => !isImplicitSymbol(p)) map (p => Parameter(p)))
        // println(s"ps: $ps")
        if (rm == null)
          ps
        else
          (selfParam :: ps)
      }

      // println(params)

      val returnTpe = Type(method.symbol.returnType)(universe)

      val nrm = if (rm == null) new RepMethod(repMethod(method).method, true) else rm

      OpsMethod(Method(name, typeParams, List(params), returnTpe, implicitParams, nrm.method.effect, method.symbol).lift, nrm)
    }

    def objMethod(method: LiftedMethod, rm: RepMethod): OpsMethod = {
      val clzName = lc.tpe.typeSymbol.name.toString
      val prefName = clzName.head.toLower + clzName.tail
      val name = if (rm != null)
        method.symbol.name.toString
      // "_" + lc.tpe.typeSymbol.name.toString + method.symbol.name.toString.capitalize + method.overloadOrder.getOrElse("")
      else
        prefName + "New"
      // "_" + lc.tpe.typeSymbol.name.toString + "New"
      val allParams = method.symbol.paramss.flatten
      val typeParams = ((lc.typeParams) ++ (method.symbol.typeParams map (_.asType))) map (Type.apply _)
      val implicitParams = (typeParams map (_.manifest)) ++ (
        ({ if (rm == null) Nil else lc.implicitParams } ++ (allParams filter isImplicitSymbol)) map (p => Parameter(p)))
      val params = (allParams filter (p => !isImplicitSymbol(p)) map (p => Parameter(p)))
      val returnTpe = Type(method.symbol.returnType)(universe)

      val nrm = if (rm == null) new RepMethod(repMethod(method).method, true) else rm

      OpsMethod(Method(name, typeParams, List(params), returnTpe, implicitParams, nrm.method.effect, method.symbol).lift, nrm, true)
    }

    def methodCaseClass(om: OpsMethod): MethodCaseClass = {
      val mom = om.method
      // val name = mom.name.drop(1)
      val name = mom.name.capitalize
      val superType = new Type("Def", mom.returnTpe.args)
      val functions = new ArrayBuffer[(Parameter, (Parameter, Parameter))]
      val params = mom.paramss.flatten flatMap { p =>
        if (p.tpe.isFunction) {
          val (inputParam, outputParam) = p.toFunction
          functions += p -> (inputParam, outputParam)
          List(inputParam, outputParam)
        } else
          List(p)

      }
      MethodCaseClass(name, mom.typeParams, params, mom.implicitParams, superType, om, functions.toMap)
    }

    val methods = lc.methods
    val repMethods = methods.filter(m => !m.symbol.isConstructor).map(repMethod)
    val opsMethods = methods.filter(m => m.symbol.isConstructor).map { a: LiftedMethod => opsMethod(a, null) } ++
      (lc.methods.filter(m => !m.symbol.isConstructor).zip(repMethods).map((opsMethod _).tupled))
    val objMethods = lm.methods.map(x => objMethod(x, repMethod(x)))
    // val repMethods = Nil
    val caseClasses = (opsMethods ++ objMethods) map (methodCaseClass _)
    // val caseClasses = Nil

    val repClass = RepClass(lc.tpe.typeSymbol.name.toString + "Rep", lc.typeParams map (Type.apply), selfParam.lift, implParams, repMethods)

    LiftedProgram(Type(lc.tpe.typeSymbol), repClass, caseClasses, opsMethods, objMethods, Type(lc.tpe.typeSymbol, true))
  }

}
