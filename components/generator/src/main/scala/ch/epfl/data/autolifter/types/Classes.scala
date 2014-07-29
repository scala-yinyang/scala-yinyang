package ch.epfl.data
package autolifter
package types

import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.api.Universe
import Utils._

case class LiftedClass(tpe: Universe#Type, methods: List[LiftedMethod], fields: List[LiftedField], typeParams: List[Universe#TypeSymbol], implicitParams: List[Universe#Symbol])

case class LiftedModule(tpe: Universe#Type, methods: List[LiftedMethod])

case class LiftedField(symbol: Universe#TermSymbol, mutable: Boolean, setter: Boolean)

case class LiftedMethod(
  symbol: Universe#MethodSymbol,
  config: Option[annotations.CMethod],
  var overloadOrder: Option[Int] = None,
  body: Option[String] = None) {
  def effect = this.config.map(_.effect) getOrElse {
    val annotations =
      if (this.symbol.isConstructor)
        this.symbol.owner.annotations
      else
        this.symbol.annotations
    import types._
    val methodEffect = annotations match {
      case Nil       => Effect.default
      case List(ann) => Effect(ann)
      case _         => sys.error("Only one annotation possible")
    }
    // FIXME only supports one annotation for each parameter, and only assume write!
    val paramsAnns = this.symbol.paramss.flatten.zipWithIndex.collect {
      case (p, i) if !p.annotations.isEmpty && Effect(p.annotations.head) == Write(0) =>
        i + 1
    }
    if (!paramsAnns.isEmpty) {
      val anns =
        if (methodEffect == Effect.default)
          paramsAnns
        else if (methodEffect == Write(0))
          0 :: paramsAnns
        else {
          sys.error("Write annotation on parameters only can occure for pure case or write case")
        }
      Write(anns: _*)
    } else
      methodEffect
  }
}

case class LiftedProgram(origClass: Type, repClass: RepClass, caseClasses: List[MethodCaseClass], opsMethods: List[OpsMethod], objMethods: List[OpsMethod], actualOrigClass: Type, component: String) {
  override def toString: String = {
    val sb = new StringBuilder
    sb ++= "Rep class:\n"
    sb ++= repClass + "\n"
    sb ++= "Case classes:\n"
    sb ++= (caseClasses mkString "\n")
    sb ++= "Ops methods:\n"
    sb ++= (opsMethods mkString "\n")
    sb.toString
  }
}

case class RepClass(name: String, typeParams: List[Type], origParam: Parameter, implicitParams: List[Parameter], methods: List[RepMethod]) {
  override def toString: String = {
    val tpePrms = typeParamsToString(typeParams)
    val implPrms = implParamsToString(implicitParams)
    val sb = new StringBuilder
    sb ++= s"$header {\n"
    sb ++= methods.mkString("  ", "\n  ", "\n")
    sb ++= "}"
    sb.toString
  }

  def header: String = {
    val tpePrms = typeParamsToString(typeParams)
    val implPrms = implParamsToString(implicitParams)
    s"implicit class $name$tpePrms($origParam)$implPrms"
  }
}

class RepMethod(val method: Method, val isConstructor: Boolean = false) {
  val correctName: String = {
    val decoded = decodedName(method.name)

    (if (isConstructor) "__new" else "") + decoded
  }
  override def toString: String = {
    method.copy(name = correctName).toString
  }
}

class ConstructorRepMethod(override val method: Method) extends RepMethod(method, true) {

}

object RepMethod {
  def apply(method: Method): RepMethod = new RepMethod(method)
}

case class OpsMethod(method: Method, repMethod: RepMethod, obj: Boolean = false)

// TODO the name functions should be changed to a more useful name
case class MethodCaseClass(name: String, typeParams: List[Type], params: List[Parameter], implicitParams: List[Parameter], superType: Option[Type], opsMethod: OpsMethod, anomalies: Map[Parameter, (List[Parameter], Parameter)]) {
  override def toString: String = {
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    val implPrms = if (implicitParams.size > 0) implicitParams.mkString("(implicit ", ", ", ")") else ""
    val prms = params.mkString("(", ", ", ")")
    s"""case class $name$tpePrms$prms$implPrms extends ${superType.get} {
      ${typeParams.zipWithIndex.map { case (p, i) => s"val m$i= manifest[$p]" }.mkString("\n")}
    }
    """
  }

  def fxCall: String = {
    import types._
    opsMethod.method.effect match {
      case Pure   => call
      case Global => s"reflectEffect($call, Global())"
      case _      => call

      /*{
        val fx = opsMethod
          .method
          .effect
          .emit(
            opsMethod.method.paramss.head map (_.variable.name))
        s"$fx($call)"
      }*/
    }
  }

  /* override def toString: String = {
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    val implPrms = if (implicitParams.size > 0) implicitParams.map(i => "val " + i).mkString("(implicit ", ", ", ")") else ""
    val prms = params.mkString("(", ", ", ")")

    val caseClassSignature = superType match {
      case Some(tp) => {
        val paramss = opsMethod.repMethod.method.paramss.map(params => params.map(p =>
          if (p.tpe.isFunction) {
            // TODO handle lambdas with more than 1 arg
            val n = p.tpe.args.size - 1
            val inputs = 1 to n map (x => s"${p.variable.inputName}$x") mkString ", "
            val tpeStr = n match {
              case 0 => "Thunk"
              case 1 => "Lambda"
              case n => "Lambda" + n
            }
            if (n == 0)
              // TODO handle in a better way
              s"$tpeStr(${p.variable.outputName})"
            else
              s"$tpeStr($inputs, ${p.variable.outputName})"
          } else if (p.tpe.isByName) {
            p.variable.outputName
          } else if (p.tpe.isInstanceOf[RepeatedType]) {
            if (Config.VAR_ARGS_LIFT) {
              s"""__varArg(${p.variable.outputName})"""
            } else {
              // TODO does not handle all cases
              val varName =
                if (p.tpe.args.head.isFunction) {
                  // FIXME this hack is because of not handling var arg functions
                  val inputsCount = p.tpe.args.head.args.size - 1
                  val n = if (inputsCount == 1) "" else inputsCount.toString
                  s"""${p.variable.name}.map(x => doLambda${n}(x))"""
                } else
                  p.variable.name
              varName + ": _*"
            }
          } else
            p.variable.name))
        val paramssString = paramss.map(l => l.mkString("List(", ",", ")")).mkString("List(", ", ", ")")
        // TODO add support for objects and the methods without caller object
        val caller =
          if (opsMethod.repMethod.isConstructor)
            "None"
          else
            "Some(self)"
        val methodSymbolName = Utils.decodedName(opsMethod.repMethod.method.name)
        val originalName =
          if (opsMethod.repMethod.isConstructor)
            "new " + methodSymbolName
          else
            methodSymbolName
        // TODO it makes sense to have another super class for constructors
        if (List("FieldDef", "FieldGetter").contains(tp.name)) {
          s"""case class $name$tpePrms$prms$implPrms extends $tp(self, "$originalName")"""
        } else if (tp.name == "FieldSetter") {
          val fieldName = opsMethod.repMethod.correctName.dropRight(2) // removes _= at the end of field name
          s"""case class $name$tpePrms$prms$implPrms extends $tp(self, "$fieldName", ${paramss.head.head})"""
        } else {
          s"""case class $name$tpePrms$prms$implPrms extends $tp($caller, "$originalName", $paramssString)"""
        }
      }
      case None => s"""case class $name$tpePrms$prms$implPrms"""
    }
    val caseClassBody = {
      val effectPart = opsMethod.method.effect match {
        case Pure => Some("override def isPure = true\n")
        case _    => None
      }
      val rebuildPart = {
        val copyInvoke = s"(copy$tpePrms _)"
        val callBody = params.size match {
          case 0 => s"(x: Any) => copy$tpePrms()"
          case 1 => copyInvoke
          case _ => s"$copyInvoke.curried"
        }
        s"override def curriedConstructor = $callBody"
      }
      (rebuildPart :: effectPart.toList).mkString("{\n    ", "\n    ", "\n  }\n")
    }
    caseClassSignature + caseClassBody
  }*/

  def call: String = {
    var paramString = paramsAccessToString(params)
    if (paramString == "") paramString = paramString + "()"
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    s"""$name$tpePrms$paramString"""
  }

  def matchString: String = {
    var paramString = paramsAccessToString(params)
    if (paramString == "") paramString = paramString + "()"
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    s"""$name$paramString"""
  }

  def matchStringMirror: String = {
    var paramString = paramsAccessToStringMirror(params)
    if (paramString == "") paramString = paramString + "()"
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    s"""$name$paramString"""
  }

  def manifests: String = {
    val manifestString = typeParams.zipWithIndex.map { case (p, i) => s"e.m$i" }.mkString(", ")
    if (manifestString == "") "()" else s"($manifestString)"
  }

  def loweredCall = call + (opsMethod.method.body match {
    case None       => ""
    case Some(body) => ".atPhase(lowering){\nimplicit def sc: SourceContext = new SourceContext{};" + body.toString + "}"
  })

  // def fxCall: String = {
  //   import types._
  //   opsMethod.method.effect match {
  //     case Pure   => call
  //     case Global => s"reflectEffect($call, Global())"
  //     case _      => call
  //   }
  // }

  def methodString(m: OpsMethod): String = {
    (if (m.obj) m.method.copy(name = m.method.name + "_obj") else m.method.toString).toString
  }

  def definition: String = {
    def anomaliesCode(anomaly: (Parameter, (List[Parameter], Parameter))): String = {
      val f = anomaly._1
      val fi = anomaly._2._1
      val fo = anomaly._2._2
      // Handle () => T case as well as the normal case A => B
      if (fi.size != 0) {
        val freshStr = fi.map(ip => "val " + ip.variable.name + " = fresh[" + ip.tpe.args.head + "]").mkString("\n    ")
        val paramStr = fi.map(ip => ip.variable.name).mkString(",")
        s"""$freshStr
    val ${fo.variable.name} = reifyEffects(${f.variable.name}(${paramStr}))"""
      } else if (f.tpe.isInstanceOf[RepeatedType] && Config.VAR_ARGS_LIFT) {
        s"""val ${fo.variable.name} = __liftSeq(${f.variable.name}.toSeq)"""
      } else {
        val parenthesis = if (f.tpe.isByName) "" else "()"
        s"""val ${fo.variable.name} = reifyEffects(${f.variable.name}$parenthesis)"""
      }
    }
    anomalies.toList match {
      // case Nil => s"${opsMethod.method} = $fxCall"
      case Nil  => s"${methodString(opsMethod)} = $call"
      case list => s"${methodString(opsMethod)} = $call"
      /*{
        val sb = new StringBuilder
        sb ++= s"${opsMethod.method} = {\n"
        sb ++= (list map anomaliesCode).mkString("    ", "\n    ", "\n    ")
        sb ++= s"$call"
        sb ++= "\n  }"
        sb.toString
      }*/
    }
  }
  def mirror: String = {
    s"case $matchString => $matchStringMirror \n" +
      s"case Reflect(e@$matchString, u, es) => reflectMirrored(Reflect($matchStringMirror$manifests, mapOver(f$$,u), f$$(es)))(mtype(manifest[A]))"
    //Reflect(e@Apply(x), u, es) => reflectMirrored(Reflect(Apply(f(x))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
  }
  def loweredDefinition: String = {
    def anomaliesCode(anomaly: (Parameter, (List[Parameter], Parameter))): String = {
      val f = anomaly._1
      val fi = anomaly._2._1
      val fo = anomaly._2._2
      // Handle () => T case as well as the normal case A => B
      if (fi.size != 0) {
        val freshStr = fi.map(ip => "val " + ip.variable.name + " = fresh[" + ip.tpe.args.head + "]").mkString("\n    ")
        val paramStr = fi.map(ip => ip.variable.name).mkString(",")
        s"""$freshStr
    val ${fo.variable.name} = reifyEffects(${f.variable.name}(${paramStr}))"""
      } else if (f.tpe.isInstanceOf[RepeatedType] && Config.VAR_ARGS_LIFT) {
        s"""val ${fo.variable.name} = __liftSeq(${f.variable.name}.toSeq)"""
      } else {
        val parenthesis = if (f.tpe.isByName) "" else "()"
        s"""val ${fo.variable.name} = reifyEffects(${f.variable.name}$parenthesis)"""
      }

    }
    anomalies.toList match {
      // case Nil => s"${opsMethod.method} = $fxCall"
      case Nil  => s"${methodString(opsMethod)} = $loweredCall"
      case list => s"${methodString(opsMethod)} = $loweredCall"
      /*{
        val sb = new StringBuilder
        sb ++= s"${opsMethod.method} = {\n"
        sb ++= (list map anomaliesCode).mkString("    ", "\n    ", "\n    ")
        sb ++= s"$call"
        sb ++= "\n  }"
        sb.toString
      }*/
    }
  }

  def emit: String = {
    val sb = new StringBuilder
    val tab = " " * 6
    sb ++= s"case $matchString => {\n"
    // println(opsMethod)
    if (opsMethod.obj) {
      val m = opsMethod.repMethod.method.name
      sb ++= s"""${tab}stream.print("val " + quote(sym) + " = " + "${opsMethod.repMethod.method.originalSymbol.owner.fullName}.$m")""" + "\n"
    } else if (opsMethod.repMethod.isConstructor) {
      val m = opsMethod.repMethod.method.name
      sb ++= s"""${tab}stream.print("val " + quote(sym) + " = new $m")""" + "\n"
    } else {
      val self = opsMethod.method.paramss.head.head.variable.name
      val m = opsMethod.repMethod.method.name
      sb ++= s"""${tab}stream.print("val " + quote(sym) + " = " + quote($self) + ".$m")""" + "\n"
    }
    val paramLists = opsMethod.repMethod.method.paramss foreach { plist =>
      sb ++= s"""${tab}stream.print("(")""" + "\n"
      var isFirst: Boolean = true
      plist.foreach(p => {
        if (isFirst) {
          isFirst = false
        } else {
          sb ++= s"""${tab}stream.print(", ")""" + "\n"
        }
        if (p.tpe.isFunction) {
          val (inputParam, outputParam) = p.toFunction
          val fi = inputParam.head.variable.name
          val fo = outputParam.variable.name
          sb ++= s"""${tab}stream.println("{ "+ quote($fi) + " => ")
${tab}emitBlock($fo)
${tab}stream.println(quote(getBlockResult($fo)))
${tab}stream.println("}")""" + "\n"
        } else {
          sb ++= s"""${tab}stream.print(quote(${p.variable.name}))""" + "\n"
        }
      })
      sb ++= s"""${tab}stream.print(")")""" + "\n"
    }
    sb ++= s"""${tab}stream.println("")""" + "\n"
    sb ++= s"    }"
    sb.toString
  }
}

case class ImplementationProgram(liftedProgram: LiftedProgram, typeContext: TypeContext) {
  def tpe = liftedProgram.origClass
  override def toString: String = {
    val sb = new StringBuilder
    sb ++= "liftedProgram:\n"
    sb ++= liftedProgram.toString + "\n"
    sb ++= "typeContext:\n"
    sb ++= typeContext + "\n"
    sb.toString
  }
}

case class TypeContext(types: List[Type], typeClasses: List[Type]) {
  def isEmpty: Boolean = types.isEmpty

  def tcsFullName: List[String] = typeClasses.map(_.name)
  def tcsModuleName: List[String] = tcsFullName.map(tc => tc.substring(tc.lastIndexOf('.') + 1))
}

object EmptyTypeContext extends TypeContext(Nil, Nil)

