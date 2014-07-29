package ch.epfl.data
package autolifter

import scala.collection.mutable.ArrayBuffer
import annotations.{ Custom, CMethod }
import scala.collection.mutable.HashMap
import types._
import scala.reflect.api.Universe
import DependencySolver._
import scala.reflect.runtime.universe.TypeTag

/**
 * This file enables auto-lifting classes via reflection.
 */
class AutoLifter(override val universe: Universe) extends Generator with UniverseContainer {

  val typeAliasResolverMap = new HashMap[String, ArrayBuffer[(Type, Type)]]()

  def autoLift[T: TypeTag]: String =
    autoLift(Custom())

  def autoLift[T](config: Custom)(implicit ttag: TypeTag[T]): String = {
    autoLiftType(config)(ttag.tpe)
  }

  def autoLiftAll[T](config: Custom)(implicit ttag: TypeTag[T]): String = {
    initTypeAliasResolverMap[T]()
    if (isTypeLifted(ttag.tpe)) ""
    else {
      markTypeLifted(ttag.tpe)
      autoLiftType(config)(ttag.tpe) + liftAllDependencies(config, this)
    }
  }

  def autoLiftType(config: Custom)(tpe: Universe#Type): String = {
    val lc = getLiftedClass(tpe.typeSymbol.asType.toType)(config)
    val lm = getLiftedModule(tpe.typeSymbol.asType.toType)(config)
    val prog = getLiftedProgram(lc, lm, config.component)
    generate(prog)
  }

  def initTypeAliasResolverMap[T]()(implicit ttag: TypeTag[T]) {
    val typeSignature = universe.typeTag[T].tpe.typeSymbol.typeSignature.asInstanceOf[universe.PolyType]
    val resultType = typeSignature.resultType.asInstanceOf[universe.ClassInfoType].normalize
    // Get types of both this class and its supertypes
    val baseClassTypes = resultType.baseClasses.map(c => resultType.baseType(c).asInstanceOf[universe.TypeRef].args)
    val superClassTypes = resultType.baseClasses.map(c => resultType.baseType(c).typeSymbol.typeSignature.typeParams)
    val superClassNames = resultType.baseClasses.map(c => c.name.toString)
    // Update map
    val data = superClassNames.zip((baseClassTypes.zip(superClassTypes)))
    data.foreach(t => {
      val name = t._1
      t._2._1.foldLeft(0)((b, a) => {
        val type1 = Type(a)(universe)
        val type2 = Type(t._2._2(b))
        if (type1 != type2) {
          val list = typeAliasResolverMap.getOrElseUpdate(name, new ArrayBuffer())
          list += new Tuple2(type1, type2)
        }
        b + 1
      })
    })
  }

  def isImplicitSymbol(s: Universe#Symbol): Boolean = s.isImplicit

  def checkMethodOwner(tpe: Universe#Type, m: Universe#MethodSymbol)(implicit config: Custom): Boolean = {
    config.parentLevel match {
      case 0 => m.owner == tpe.typeSymbol
      case 1 => List(universe.typeOf[Any].typeSymbol, universe.typeOf[AnyRef].typeSymbol).forall(_ != m.owner)
      case _ => true
    }
  }

  def methodShouldBeLifted(tpe: Universe#Type, m: Universe#MethodSymbol)(implicit config: Custom): Boolean = {
    // TODO: Rewrite conditions (there is overlap here)
    (methodNotInExcludedList(m) &&
      ((checkMethodOwner(tpe, m) && !m.isGetter && !m.isSetter && methodIsInIncludedList(m) && !m.isSynthetic) || (!config.includedMethods.isEmpty && methodIsInIncludedList(m))) &&
      /* Fix for case where a constructor is given -- ignore <init> then */
      !(tpe.members.toList.filter((m: Universe#Symbol) => m.isConstructor).size > 1 && m.name.toString == "$init$") && !(m.isConstructor && tpe.typeSymbol.isAbstract))
  }

  def methodNotInExcludedList(m: Universe#MethodSymbol)(implicit config: Custom): Boolean = config.excludedMethods match {
    case Nil  => true
    case list => !list.exists(x => x == m.name.decoded)
  }

  def methodIsInIncludedList(m: Universe#MethodSymbol)(implicit config: Custom): Boolean = config.includedMethods match {
    case Nil  => true
    case list => list.exists(x => x.name == m.name.decoded) && methodSpecifications(m)
  }

  def methodConfig(m: Universe#MethodSymbol)(implicit config: Custom): Option[CMethod] = {
    val methods = config.includedMethods.filter(_.name == m.name.decoded)
    methods match {
      case Nil                                   => None
      case l @ List(x) if l.head.paramss.isEmpty => Some(x)
      case l => {
        // TODO: Check equality of implicits as well?
        val res = l filter (x => {
          val params = (if (x.paramss.isEmpty) List() else x.paramss.head) map (_.tpe.toString)
          val params2 = (if (m.paramss.isEmpty) List() else m.paramss.head) map (_.typeSignature.toString)
          params equals params2
        })
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
      if (config.includedMethods.isEmpty)
        true
      else {
        !methodConfig(m).isEmpty
      }
    varArg && correctOverload
  }

  def getMethods(tpe: Universe#Type, tree: Option[Universe#Tree] = None)(implicit config: Custom): List[LiftedMethod] = {
    val methods = tpe.members.toList.reverse
      .filter((m: Universe#Symbol) => m.isMethod && methodShouldBeLifted(tpe, m.asMethod))
      .map(m => LiftedMethod(m.asMethod, methodConfig(m.asMethod)))

    // adding the overload number to each method
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

  def fieldShouldBeLifted(tpe: Universe#Type, v: Universe#Symbol)(implicit config: Custom): Boolean = {
    // println("in method fieldShouldBeLifted symbol §" + v.name.decoded + "§ encoded §" + v.name.encoded + "§toString: §" + v.name.toString + "§")
    config.excludedFields match {
      case Nil  => true
      case list => !list.exists(x => x.name == v.name.decoded || x.name + " " == v.name.decoded || x.name + "_=" == v.name.decoded)
    }
  }

  def getLiftedClass(tpe: Universe#Type)(implicit config: Custom): LiftedClass = {
    val implicitParameters = (tpe.members filter (m => isImplicitSymbol(m))).toList
    val typeParams = if (tpe.typeSymbol.typeSignature.takesTypeArgs)
      tpe.typeSymbol.typeSignature.asInstanceOf[Universe#PolyType].typeParams map (_.asType)
    else
      Nil

    val methods = getMethods(tpe)
    val fields = tpe.members filter { (m: Universe#Symbol) => m.isTerm } collect {
      case v if v.asTerm.isGetter && fieldShouldBeLifted(tpe, v) => {
        val mutable = v.asTerm.setter.isTerm
        LiftedField(v.asTerm, mutable, false)
      }
      case v if v.asTerm.isVal && fieldShouldBeLifted(tpe, v) && !v.asTerm.getter.isTerm => {
        // parameters
        LiftedField(v.asTerm, false, false)
      }
      case v if v.asTerm.isSetter && fieldShouldBeLifted(tpe, v) =>
        LiftedField(v.asTerm, true, true)
    }
    val originalTpe = Utils.getOriginalType(tpe)
    val clazz = LiftedClass(originalTpe, methods, fields.toList, typeParams, implicitParameters)
    clazz
  }

  def getLiftedModule(tpe: Universe#Type)(implicit config: Custom): LiftedModule = {
    val methods =
      getMethods(tpe.typeSymbol.companionSymbol.typeSignature)
        .filterNot(m => m.symbol.asMethod.isConstructor)
    val module = LiftedModule(tpe, methods.toList)

    module
  }

  def modifyType(y: Type, cond: Type => Boolean, modifyName: String => String): Type = {
    if (cond(y)) y.modify(modifyName(y.name), y.args map (a => modifyType(a, cond, modifyName)))
    else y.modify(y.name, y.args map (a => modifyType(a, cond, modifyName)))
  }

  def modifyType(y: Type, cond: Type => Boolean): Type = {
    modifyType(y, cond, n => "_" + n)
  }

  def getLiftedProgram(lc: LiftedClass, lm: LiftedModule, component: String): LiftedProgram = {
    val selfParam = Parameter(Variable("self"), Type(lc.tpe.typeSymbol))
    val classTypeParams = lc.typeParams map (Type.apply _)

    def isInTypeList(y: Type, l: List[Type]): Boolean = l.map(x => x.name).contains(y.name)

    def getMethodTypeParams(method: LiftedMethod) =
      methodTypeForLiftedMethod(method).typeParams.map(_.asType).map(Type.apply _)

    def filterMethodTypeParams(methodTypeParams: List[Type]) =
      methodTypeParams map (p => modifyType(p, x => isInTypeList(x, classTypeParams)))

    def methodTypeForLiftedMethod(lm: LiftedMethod): universe.Type =
      lm.symbol.asInstanceOf[universe.Symbol].infoIn(lc.tpe.asInstanceOf[universe.Type]).asInstanceOf[universe.Type]

    def repMethod(method: LiftedMethod): RepMethod = {
      val name = if (method.symbol.isConstructor)
        lc.tpe.typeSymbol.name.toString
      else
        method.symbol.name.toString
      val methodType = methodTypeForLiftedMethod(method)
      val allParams = methodType.paramss.flatten
      val typeParams = filterMethodTypeParams(getMethodTypeParams(method))
      val existingImplicitParams = allParams filter isImplicitSymbol map Parameter.apply
      // duplication of Manifest[T] is removed in the next line
      val tp = typeParams map (_.manifest)
      val implicitParams = tp ++ (existingImplicitParams filter (man => !tp.contains(man))) ++
        (
          method.overloadOrder map { i =>
            Parameter(Variable(s"overload$i"), new Type(s"Overloaded$i", Nil))
          }).toList
      val paramss = methodType.paramss filter (ps => ps exists (p => !isImplicitSymbol(p))) map (l => l map (p => Parameter.apply(p)))
      val returnTpe = Type(methodType.resultType)(universe)
      val effect = method.effect
      val m = Method(name, typeParams, paramss, returnTpe, implicitParams, effect, method.symbol, method.body)
      RepMethod(m.lift)
    }

    def repField(field: LiftedField): RepMethod = {
      val name = field.symbol.name.toString
      val returnTpe = {
        val tpe = Type(field.symbol.asInstanceOf[universe.Symbol].typeSignatureIn(lc.tpe.asInstanceOf[universe.Type]))(universe)
        if (tpe.isInstanceOf[RepeatedType])
          new Type("Seq", tpe.args)
        else
          tpe
      }
      val m = field match {
        case LiftedField(_, true, true) => {
          // TODO as soon as we have the notions of effects it should have mutation effect
          Method(name, Nil, List(List(Parameter(field.symbol.asMethod.paramss.head.head))), returnTpe, Nil, Effect.default, field.symbol)
        }
        case _ => {
          val effect =
            if (field.mutable)
              Effect.default
            else
              Pure
          Method(name, Nil, Nil, returnTpe, Nil, effect, field.symbol)
        }
      }
      RepMethod(m.lift)
    }

    def opsMethod(method: LiftedMethod, rm: RepMethod): OpsMethod = {

      // Set name
      val clzName = lc.tpe.typeSymbol.name.toString
      val prefName = clzName.head.toLower + clzName.tail
      val name = if (rm != null)
        prefName + method.symbol.name.toString.capitalize + method.overloadOrder.getOrElse("")
      else
        prefName + "New" + method.overloadOrder.getOrElse("")
      // If a class contains type parameters A,B and there is a method with
      // another B, we should rename the method type parameter (for example
      // consider the method collectFirst in HashMap). The following helpers
      // handle this case

      val methodType = methodTypeForLiftedMethod(method)

      // Set type params
      val methodTypeParams = getMethodTypeParams(method)
      val typeParams = classTypeParams ++ filterMethodTypeParams(methodTypeParams)
      // Set args
      val args = methodType.paramss.flatten

      val params = {
        val pso = (args filter (p => !isImplicitSymbol(p)) map (p => Parameter(p)))
        val ps = pso map (y => {
          new Parameter(y.variable, modifyType(y.tpe, x => isInTypeList(x, classTypeParams) && isInTypeList(x, methodTypeParams)), y.isDefault)
        })
        if (rm != null) selfParam :: ps else ps
      }

      // Set implicit params & avoid including the same manifest twice in impliit parameters
      val implParamsDeclared = typeParams map (_.manifest)
      val implParamsImplicitly = (args filter isImplicitSymbol) map (p => Parameter(p)) filter (p => !implParamsDeclared.contains(p))
      val implParamsClass = ({
        if (rm == null) Nil
        else lc.implicitParams
      }) map (p => Parameter(p)) filter (p => !implParamsDeclared.contains(p))
      val implicitParams = implParamsDeclared ++ implParamsImplicitly ++ implParamsClass

      // val ownerName = method.symbol.owner.name.toString
      // val returnTpe = modifyType(Type(method.symbol.returnType)(universe),
      //   t => typeAliasResolverMap.contains(ownerName),
      //   n => typeAliasResolverMap.get(ownerName).get.find(e => e._2.name == n) match {
      //     case Some(x) => x._1.toString;
      //     case None    => n
      //   })
      val returnTpe = Type(methodType.resultType)(universe)

      val nrm =
        if (rm == null) {
          val rmMethod = repMethod(method).method
          new ConstructorRepMethod(rmMethod.copy(typeParams = typeParams, implicitParams = rmMethod.implicitParams ++ implParamsDeclared))
        } else
          rm

      OpsMethod(Method(name, typeParams, List(params), returnTpe, implicitParams, nrm.method.effect, method.symbol, method.body).lift, nrm)
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
      val typeParams = ((method.symbol.typeParams map (_.asType))) map (Type.apply _)
      val implicitParams = (typeParams map (_.manifest)) ++ (
        ({ if (rm == null) Nil else lc.implicitParams } ++ (allParams filter isImplicitSymbol)) map (p => Parameter(p)))
      val params = (allParams filter (p => !isImplicitSymbol(p)) map (p => Parameter(p)))
      val returnTpe = Type(method.symbol.returnType)(universe)

      val nrm = if (rm == null) new RepMethod(repMethod(method).method, true) else rm

      OpsMethod(Method(name, typeParams, List(params), returnTpe, implicitParams, nrm.method.effect, method.symbol).lift, nrm, true)
    }

    def opsField(field: LiftedField, rf: RepMethod): OpsMethod = {
      val clzName = lc.tpe.typeSymbol.name.toString
      val prefName = clzName.head.toLower + clzName.tail
      val name =
        prefName + "_Field_" + field.symbol.name.toString.capitalize
      val params = rf.method.paramss match {
        case List(List(x)) => List(List(selfParam.lift, x))
        case _             => List(List(selfParam.lift))
      }
      OpsMethod(Method(name, classTypeParams, params, rf.method.returnTpe, classTypeParams map (_.manifest), rf.method.effect, field.symbol), rf)
    }

    def methodCaseClass(om: OpsMethod): MethodCaseClass = {
      val mom = om.method
      val name = mom.name.capitalize
      val superType = new Type("Def", mom.returnTpe.args)
      val anomalies = new ArrayBuffer[(Parameter, (List[Parameter], Parameter))]
      val params = mom.paramss.flatten flatMap { p =>
        if (p.tpe.isFunction && false) {
          // FIXME here we should also assume the functions inside a var arg parameter
          val (inputParam, outputParam) = p.toFunction
          anomalies += p -> (inputParam, outputParam)
          inputParam ++ List(outputParam)
        } else if (p.tpe.isByName) {
          val outputParam = p.toByName
          anomalies += p -> (Nil, outputParam)
          List(outputParam)
        } else if (p.tpe.isInstanceOf[RepeatedType] && Config.VAR_ARGS_LIFT) {
          val outputParam = p.toVarArg
          anomalies += p -> (Nil, outputParam)
          List(outputParam)
        } else {
          List(p)
        }

      }
      MethodCaseClass(name, mom.typeParams, params, mom.implicitParams, Some(superType), om, anomalies.toMap)
    }

    def fieldCaseClass(om: OpsMethod): MethodCaseClass = {
      val mom = om.method
      val name = mom.name.capitalize
      sealed trait Kind
      case object Immutable extends Kind
      case object Setter extends Kind
      case object Getter extends Kind
      val kind = if (om.method.originalSymbol.asTerm.isSetter)
        Setter
      else if (om.method.originalSymbol.asTerm.setter.isTerm)
        Getter
      else
        Immutable
      val superType = kind match {
        case Setter    => new Type("Def", mom.paramss.head.apply(1).tpe.args)
        case Getter    => new Type("Def", mom.returnTpe.args)
        case Immutable => new Type("Def", mom.returnTpe.args)
      }
      // val superTypeName =
      //   if (om.method.originalSymbol.asTerm.isSetter)
      //     "FieldSetter"
      //   else if (om.method.originalSymbol.asTerm.setter.isTerm)
      //     "FieldGetter"
      //   else
      //     "FieldDef"
      // val superType = new Type(superTypeName, mom.returnTpe.args)
      MethodCaseClass(name, mom.typeParams, mom.paramss.flatten, mom.implicitParams, Some(superType), om, Map())
    }

    val repMethods = lc.methods filter (m => !m.symbol.isConstructor) map repMethod
    val opsMethods =
      (lc.methods.filter(m => m.symbol.isConstructor).map({ a: LiftedMethod => opsMethod(a, null) })) ++
        (lc.methods.filter(m => !m.symbol.isConstructor).zip(repMethods).map((opsMethod _).tupled))

    val implParams = (classTypeParams map (_.manifest)) ++ (lc.implicitParams map Parameter.apply)
    val objMethods = lm.methods.map(x => objMethod(x, repMethod(x)))
    val caseClasses = (opsMethods ++ objMethods) map (methodCaseClass _)

    val repFields = lc.fields map repField
    val opsFields = lc.fields zip repFields map ((opsField _).tupled)
    // TODO objMethods
    val caseClassesFields = opsFields map (fieldCaseClass _)

    val repClass = RepClass(lc.tpe.typeSymbol.name.toString + "Rep", lc.typeParams map (Type.apply), selfParam.lift, implParams, repMethods ++ repFields)
    val res = LiftedProgram(Type(lc.tpe.typeSymbol), repClass, caseClasses ++ caseClassesFields, opsMethods ++ opsFields, objMethods, Type(lc.tpe.typeSymbol, true), component)
    println(res.actualOrigClass)
    res
  }

}
