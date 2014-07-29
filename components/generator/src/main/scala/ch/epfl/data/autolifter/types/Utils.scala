package ch.epfl.data
package autolifter
package types

import scala.reflect.runtime.{ universe => runtimeUniverse }
import scala.reflect.api.Universe

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

  def decodedName(name: String): String =
    runtimeUniverse.TermName(name).decodedName.toString

  def paramsAccessToString(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l => l.map(p => {
        // If a parameter is of repeat type, we should add "_*" to it
        p.variable.name + {
          if (p.tpe.isInstanceOf[RepeatedType]) ":_*"
          else ""
        }
      }).mkString("(", ", ", ")")
    }

  def paramsAccessToStringMirror(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l => l.map(p => {
        // If a parameter is of repeat type, we should add "_*" to it
        val s = p.variable.name + {
          (if (p.tpe.isInstanceOf[RepeatedType]) ":_*"
          else "")
        }
        if (p.tpe.isFunction) s else "f$(" + s + ")"
      }).mkString("(", ", ", ")")
    }

  def implParamsToString(params: List[Parameter]): String =
    params match {
      case Nil => ""
      case l   => l.mkString("(implicit ", ", ", ")")
    }

  /**
   * In the case of being a mirror type, returns the corresponding original type
   */
  def getOriginalType[U <: Universe](tpe: U#Type): U#Type = {
    val mirrorType = getMirrorAnnotation(tpe)
    val originalType = mirrorType match {
      case Some(mt) => mt.typeArgs.head
      case None     => tpe
    }
    originalType
  }

  def isMirrorType[U <: Universe](tpe: U#Type): Boolean = {
    getMirrorAnnotation(tpe).nonEmpty
  }

  private def getMirrorAnnotation[U <: Universe](tpe: U#Type): Option[U#Type] = {
    val classAnnotations = tpe.typeSymbol.annotations
    classAnnotations.map(_.tree.tpe) collectFirst {
      case t if t.typeSymbol.name.toString == "reflect" => t
    }
  }
}
