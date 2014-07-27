package ch.epfl.lamp
package autolifter
package types

import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.api.Universe
import Utils._
import scala.reflect.ClassTag

trait Liftable {
  type Self
  def lift: Self
}

class Type(val name: String, val args: List[Type]) extends Liftable {
  type Self = Type
  def lift = new Type("Rep", List(this))
  val isTrait: Boolean = false

  override def toString: String = {
    s"""$name${typeParamsToString(args)}"""
    // args match {
    //      case Nil => name
    //      case _ => s"""$name[${args mkString ", "}]"""
    // }
  }

  def =:=(t: Type): Boolean = (name equals t.name) && (args.length == t.args.length)

  override def hashCode(): Int = name.hashCode()

  override def equals(o: Any): Boolean = {
    val t = o.asInstanceOf[Type]
    (name equals t.name) && (args equals t.args)
  }

  def isFunction: Boolean = false
  def isByName: Boolean = false

  def manifest: Parameter = {
    Parameter(Variable(s"manifest$flattenName"), new Type("Manifest", List(this)))
  }

  def flattenName: String = {
    name + (args map (_.flattenName) mkString "$")
  }
}

class ByNameType(val tpe: Type) extends Type("=>", List(tpe)) {
  override def isByName: Boolean = true
}

object Type {
  def apply[U <: Universe](tpe: Universe#Type)(implicit universe: Universe): Type = {
    // if (tpe.isInstanceOf[Universe#TypeRef]) {
    //   val tr = tpe.asInstanceOf[Universe#TypeRef]
    //   if (tpe <:< Universe#typeOf[(_ => _)] && tr.typeConstructor.typeSymbol.name.toString == "Function1")
    //     new FunctionType(apply(tr.args.head), apply(tr.args.tail.head))
    //   else
    //     new Type(tr.typeConstructor.typeSymbol.name.toString, tr.args map apply)
    // } else {
    //   null
    // }
    tpe match {
      case tr @ universe.TypeRef(pre, sym, args) => {
        //val tr = tpe.asInstanceOf[universe.TypeRef]
        if (tr.typeConstructor.typeSymbol.name.toString == "Function1")
          new FunctionType(apply(tr.args.head), apply(tr.args.tail.head))
        else if (tr.typeConstructor.typeSymbol.name.toString.startsWith("Function"))
          new FunctionNType(tr.args.dropRight(1) map apply, apply(tr.args.last))
        else {
          val name = tr.typeConstructor.typeSymbol.name.toString
          if (name == "<byname>")
            new ByNameType(apply(args.head))
          else {
            val typeSymbol = tr.typeConstructor.typeSymbol.asType
            new Type(typeSymbol.name.toString, tr.args map apply) {
              override val isTrait = if (typeSymbol.isClass) typeSymbol.asClass.isTrait else false
            }
          }
        }
      }
      case tt @ universe.ThisType(pre) => {
        // throw new RuntimeException("ThisTYpe ! ")
        apply(pre)
      }
      case _ => null
    }
  }

  def apply(s: Universe#Symbol, fullName: Boolean): Type = {
    if (s.isType) {
      val ts = s.asType
      val name =
        if (fullName)
          s.fullName
        else
          s.name.toString
      val typeSymbol = s.asType
      new Type(name, ts.typeParams map (apply _)) {
        override val isTrait = if (typeSymbol.isClass) typeSymbol.asClass.isTrait else false
      }
    } else {
      null
    }
  }

  def apply(s: Universe#Symbol): Type = {
    apply(s, false)
  }
}

class FunctionType(val input: Type, override val output: Type) extends FunctionNType(List(input), output) {
  override def lift = new FunctionType(input.lift, output.lift)

  override def toString: String = {
    s"""($input => $output)"""
  }
}

class FunctionNType(val inputs: List[Type], val output: Type) extends Type("Function" + inputs.length, inputs :+ output) {
  override def lift = new FunctionNType(inputs map (_.lift), output.lift)

  override def toString: String = {
    s"""((${inputs.mkString(",")}) => $output)"""
  }

  override def isFunction: Boolean = true
}

case class Variable(name: String) {
  override def toString: String = name
}

case class Parameter(variable: Variable, tpe: Type, isDefault: Boolean = false) extends Liftable {
  type Self = Parameter
  override def toString: String = s"$variable: $tpe"
  def lift = Parameter(variable, tpe.lift)

  def name: String = variable.name

  def toFunction: (Parameter, Parameter) = {
    val tpe = this.tpe.asInstanceOf[FunctionType]
    val inputTpe = tpe.input
    val outputTpe = tpe.output
    val fTpe = new Type("Sym", inputTpe.args)
    val sTpe = new Type("Block", outputTpe.args)
    val fName = variable.name + "Input"
    val sName = variable.name + "Output"
    val inputParam = Parameter(Variable(fName), fTpe)
    val outputParam = Parameter(Variable(sName), sTpe)
    (inputParam, outputParam)
  }
}

object Parameter {
  def apply(p: Universe#Symbol): Parameter = {
    // println(s"symbol: $p")
    val variable = Variable(p.name.toString.trim)
    val tpe = Type(p.typeSignature)(runtimeUniverse)
    val isDefault = p.asTerm.isParamWithDefault
    Parameter(variable, tpe, isDefault)
  }
}

case class Method(name: String, typeParams: List[Type], paramss: List[List[Parameter]], returnTpe: Type, implicitParams: List[Parameter], effect: Effect, originalSymbol: Universe#Symbol) extends Liftable {
  type Self = Method
  lazy val paramString = paramss map (l => paramsToString(l)) mkString ""
  lazy val argString = paramss map (l => argsToString(l)) mkString ""
  lazy val implString = implParamsToString(implicitParams)
  lazy val typeString = typeParamsToString(typeParams)
  override def toString: String = {
    s"""def $name$typeString$paramString$implString: $returnTpe"""
  }

  def call: String = {
    val typeString = typeParamsToString(typeParams)
    val paramString = paramss map (l => paramsAccessToString(l)) mkString ""
    val implString = paramsAccessToString(implicitParams)
    s"""$name$typeString$paramString$implString"""
  }

  def lift = this.copy(paramss = paramss map (l => l.map(p => p.lift)), returnTpe = returnTpe.lift)
}
