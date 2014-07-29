package ch.epfl.data
package autolifter
package types

import scala.reflect.api.Universe

// from Forge
trait Effect {
  def emit(params: List[String]): String = "reflect" + toString
}

object Effect {
  def apply(ann: Universe#Annotation): Effect = ann.tpe.typeSymbol.name.toString match {
    case "pure"    => Pure
    case "mutable" => Mutable
    case "write"   => Write(0)
    case "simple"  => Simple
    case "global"  => Global
    case _         => null
  }
  def default: Effect = Global
}

case object Pure extends Effect
case object Mutable extends Effect
case object Simple extends Effect {
  override def emit(params: List[String]): String = "reflectEffect"
}
case class Write(args: Int*) extends Effect {
  override def emit(params: List[String]): String = "reflectWrite(" + args.map(params.toArray.apply _).mkString(", ") + ")"
}
case object Global extends Effect
