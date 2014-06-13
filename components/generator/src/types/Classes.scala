package ch.epfl.lamp
package autolifter
package types

import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.api.Universe
import Utils._

case class LiftedClass(tpe: Universe#Type, methods: List[LiftedMethod], fields: List[Universe#TermSymbol], typeParams: List[Universe#TypeSymbol], implicitParams: List[Universe#Symbol])

case class LiftedModule(tpe: Universe#Type, methods: List[LiftedMethod])

case class LiftedMethod(symbol: Universe#MethodSymbol, config: Option[annotations.CMethod], var overloadOrder: Option[Int] = None) {
  def effect = this.config.map(_.effect) getOrElse {
    val annotations =
      if (this.symbol.isConstructor)
        this.symbol.owner.annotations
      else
        this.symbol.annotations
    import types._
    val methodEffect = annotations match {
      case Nil       => Pure
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
        if (methodEffect == Pure)
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

case class LiftedProgram(origClass: Type, repClass: RepClass, caseClasses: List[MethodCaseClass], opsMethods: List[OpsMethod], actualOrigClass: Type) {
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

// case class RepMethod(name: String, typeParams: List[ru.TypeSymbol], paramLists: List[List[ru.Symbol]], returnTpe: ru.Type, implicitParams: List[(ru.TermSymbol, ru.TypeSymbol)])
class RepMethod(val method: Method, val isConstructor: Boolean = false) {
  override def toString: String = {
    method.copy(name = runtimeUniverse.TermName(method.name).decodedName.toString).toString
  }
}

object RepMethod {
  def apply(method: Method): RepMethod = new RepMethod(method)
}

// case class OpsMethod(name: String, typeParams: List[ru.TypeSymbol], params: List[ru.Symbol], returnTpe: ru.Type, implicitParams: List[ru.Symbol])
case class OpsMethod(method: Method, repMethod: RepMethod)

// case class MethodCaseClass(name: String, typeParams: List[ru.TypeSymbol], params: List[ru.Symbol], returnTpe: ru.Type, implicitParams: List[ru.Symbol])
case class MethodCaseClass(name: String, typeParams: List[Type], params: List[Parameter], implicitParams: List[Parameter], superType: Type, opsMethod: OpsMethod, functions: Map[Parameter, (Parameter, Parameter)]) {
  override def toString: String = {
    val tpePrms = typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }
    val implPrms = if (implicitParams.size > 0) implicitParams.mkString("(implicit ", ", ", ")") else ""
    val prms = params.mkString("(", ", ", ")")
    s"""case class $name$tpePrms$prms$implPrms extends $superType"""
  }

  def call: String = {
    val paramString = paramsAccessToString(params)
    s"""$name$paramString"""
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

  def definition: String = {
    def functionCode(func: (Parameter, (Parameter, Parameter))): String = {
      val f = func._1
      val fi = func._2._1
      val fo = func._2._2
      s"""val ${fi.variable.name} = fresh[${fi.tpe.args.head}]
  val ${fo.variable.name} = reifyEffects(${f.variable.name}(${fi.variable.name}))"""
    }
    functions.toList match {
      case Nil => s"${opsMethod.method} = $fxCall"
      case list => {
        val sb = new StringBuilder
        sb ++= s"${opsMethod.method} = {\n"
        sb ++= (list map functionCode).mkString("    ", "\n    ", "\n    ")
        sb ++= s"reflectEffect($call, summarizeEffects(${list.head._2._2.variable.name}).star)" // TODO Generalize to
        sb ++= "\n  }"
        sb.toString
      }
    }
  }

  def emit: String = {
    val sb = new StringBuilder
    val tab = " " * 6
    sb ++= s"case $call => {\n"
    // println(opsMethod)
    if (opsMethod.repMethod.isConstructor) {
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
          val fi = inputParam.variable.name
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
