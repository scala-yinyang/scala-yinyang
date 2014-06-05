package ch.epfl.lamp
package autolifter
package types

object Utils {

  def typeParamsToString(typeParams: List[Type]): String =
    typeParams match {
      case Nil => ""
      case l   => l.mkString("[", ", ", "]")
    }

  def paramsToString(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l   => l.mkString("(", ", ", ")")
    }

  def paramsAccessToString(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l   => l.map(_.variable.name).mkString("(", ", ", ")")
    }

  def implParamsToString(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l   => l.mkString("(implicit ", ", ", ")")
    }
}