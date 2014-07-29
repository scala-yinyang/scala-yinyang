package ch.epfl.data
package autolifter
package types

import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.api.Universe
import annotations.{ Custom, CMethod }
import Utils._
import scala.reflect.ClassTag
import DependencySolver._

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
  def modify(newName: String, newArgs: List[Type]): Type = new Type(newName, newArgs)
}

class ByNameType(val tpe: Type) extends Type("=>", List(tpe)) {
  override def lift = new ByNameType(tpe.lift)
  override def toString: String = "=> " + tpe.toString
  override def isByName: Boolean = true
  override def modify(newName: String, newArgs: List[Type]): Type = new ByNameType(newArgs.head)
}

class RepeatedType(val tpe: Type) extends Type("repeated", List(tpe)) {
  override def lift =
    // if (Config.VAR_ARGS_LIFT)
    //   (new Type("Seq", List(tpe))).lift
    // else
    new RepeatedType(tpe.lift)
  override def toString = tpe.toString + "*"
  override def modify(newName: String, newArgs: List[Type]): Type = new RepeatedType(newArgs.head)
}

class RefinedType(val tpe: List[Type]) extends Type("refined", tpe)

object Type {
  def apply[U <: Universe](tpe: Universe#Type)(implicit universe: Universe): Type = {
    registerDependantType(tpe)
    tpe match {
      case tr @ universe.TypeRef(pre, sym, args) => {
        val name = tr.typeConstructor.typeSymbol.name.toString
        name match {
          case "Function1" => new FunctionType(apply(tr.args.head), apply(tr.args.tail.head))
          case x if x.startsWith("Function") =>
            new FunctionNType(tr.args.dropRight(1) map apply, apply(tr.args.last))
          case "<repeated>" =>
            new RepeatedType(apply(tr.args.head))
          case dflt @ _ =>
            if (name == "<byname>")
              new ByNameType(apply(args.head))
            else {
              val typeSymbol =
                if (Utils.isMirrorType(tpe))
                  Utils.getOriginalType(tpe).typeSymbol.asType
                else
                  tr.typeConstructor.typeSymbol.asType
              new Type(typeSymbol.name.toString, tr.args map apply) {
                override val isTrait = if (typeSymbol.isClass) typeSymbol.asClass.isTrait else false
              }
            }
        }
      }
      case tt @ universe.ThisType(pre)                   => apply(pre)
      case tt @ universe.NullaryMethodType(tpe)          => apply(tpe)
      // Note: the next lines are highly experimental (used for collections)!
      case tt @ universe.AnnotatedType(annotations, tpe) => apply(tpe)
      case tt @ universe.RefinedType(args, scope)        => apply(args.head)
      case tt @ universe.ExistentialType(types, stype)   => apply(stype)
      case tt @ universe.PolyType(smb, tpe)              => apply(tpe)
      case tt @ universe.MethodType(smb, tpe)            => apply(tpe)
      case dflt @ _                                      => throw new Exception(dflt.getClass.toString)
    }
  }

  def apply(s: Universe#Symbol, fullName: Boolean): Type = {
    if (s.isType) {
      val ts = {
        s.asType
      }
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
  override def lift =
    if (Config.INLINE_FUNCTIONS)
      new FunctionType(input.lift, output.lift)
    else
      super.lift

  override def toString: String = {
    s"""($input => $output)"""
  }
}

class FunctionNType(val inputs: List[Type], val output: Type) extends Type("Function" + inputs.length, inputs :+ output) {
  override def lift =
    if (Config.INLINE_FUNCTIONS)
      new FunctionNType(inputs map (_.lift), output.lift)
    else super.lift

  override def toString: String = {
    s"""((${inputs.mkString(",")}) => $output)"""
  }

  override def isFunction: Boolean = true
  override def modify(newName: String, newArgs: List[Type]): Type = new FunctionNType(newArgs.dropRight(1), newArgs.last)
}

case class Variable(name: String) {
  override def toString: String = name
  def inputName: String = name + "Input"
  def outputName: String = name + "Output"
}

case class Parameter(variable: Variable, tpe: Type, isDefault: Boolean = false) extends Liftable {
  type Self = Parameter
  override def toString: String = s"$variable : $tpe"
  def lift = Parameter(variable, tpe.lift)
  override def equals(a: Any) = {
    if (a.isInstanceOf[Parameter])
      a.asInstanceOf[Parameter].tpe == this.tpe
    else super.equals(a)
  }

  def name: String = variable.name

  def toFunction: (List[Parameter], Parameter) = {
    val fName = variable.inputName
    val sName = variable.outputName
    val tpe = {
      if (this.tpe.isInstanceOf[FunctionType])
        this.tpe.asInstanceOf[FunctionType]
      else this.tpe.asInstanceOf[FunctionNType]
    }
    val outputTpe = {
      val otpe = new Type("Block", tpe.output.args)
      Parameter(Variable(sName), otpe)
    }
    var count = 0
    val inputTpe = tpe.inputs.map(p => {
      count += 1
      val itpe = new Type("Sym", p.args)
      Parameter(Variable(fName + count), itpe)
    })
    (inputTpe, outputTpe)
  }

  def toByName: Parameter =
    Parameter(Variable(variable.outputName), new Type("Block", tpe.args.head.args))

  def toVarArg: Parameter = {
    val SeqT = new Type("Seq", tpe.args.head.args)
    Parameter(Variable(variable.outputName), SeqT.lift)
  }

}

object Parameter {
  def apply(p: Universe#Symbol): Parameter = {
    val variable = Variable(p.name.toString.trim)
    val tpe = Type(p.typeSignature)(runtimeUniverse)
    val isDefault = p.asTerm.isParamWithDefault
    Parameter(variable, tpe, isDefault)
  }
}

case class Method(
  name: String,
  typeParams: List[Type],
  paramss: List[List[Parameter]],
  returnTpe: Type,
  implicitParams: List[Parameter],
  effect: Effect,
  originalSymbol: Universe#Symbol,
  body: Option[String] = None)(implicit config: Custom) extends Liftable {
  type Self = Method
  override def toString: String = {
    val typeString = typeParamsToString(typeParams)
    val paramString = paramss map (l => paramsToString(l)) mkString ""
    val implString = implParamsToString(implicitParams)
    val overrideModifier = if (config.overrideMethods.contains(name)) "override" else ""
    if (paramss.size == 0 && implString.size == 0 && !originalSymbol.asTerm.isGetter)
      s"""$overrideModifier def $name$typeString$paramString$implString(): $returnTpe"""
    else
      s"""$overrideModifier def $name$typeString$paramString$implString: $returnTpe"""
  }

  def call: String = {
    val typeString = typeParamsToString(typeParams)
    val paramString = paramss map (l => paramsAccessToString(l)) mkString ""
    val implString = paramsAccessToString(implicitParams)
    s"""$name$typeString$paramString$implString"""
  }

  def lift = this.copy(paramss = paramss map (l => l.map(p => p.lift)), returnTpe = returnTpe.lift)
}
