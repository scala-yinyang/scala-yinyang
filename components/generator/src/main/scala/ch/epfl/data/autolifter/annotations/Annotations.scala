package ch.epfl.data
package autolifter
package annotations

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{ universe => ru }

//TODO: Virtualize toString (e.g. __toString)
final case class Custom(
  component: String = "DeepDSL",
  includedMethods: List[CMethod] = Nil,
  excludedMethods: List[String] = List("getClass", "toString", "hashCode"),
  overrideMethods: List[String] = List("clone"),
  excludedFields: List[CMethod] = Nil,
  parentLevel: Int = 1) extends StaticAnnotation

/**
 * Annotation that marks that the class has a deep embedding.
 */
final case class deep() extends StaticAnnotation

final case class metadeep(folder: String, imports: String, component: String) extends StaticAnnotation

/**
 * Represents a class which represents the interface of another class
 * The main use case is for effect annotation for standard library
 */
final case class reflect[T]() extends StaticAnnotation

object Custom {
  implicit val defaultCustom: Custom = Custom()
}

final case class CMethod(name: String, paramss: List[List[CType]] = Nil, effect: types.Effect = types.Effect.default) extends StaticAnnotation
final case class CType(tpe: ru.Type) extends StaticAnnotation
object CType extends StaticAnnotation {
  def apply[T: ru.TypeTag]: CType = CType(implicitly[ru.TypeTag[T]].tpe)
}

trait EffectAnnotation extends StaticAnnotation

// for a method
case class pure() extends EffectAnnotation
// for a class (makes its constructor mutable)
case class mutable() extends EffectAnnotation
// for a method
case class simple() extends EffectAnnotation
// for a method and a parameter of a method
//   if it's applied to a method, makes `this` a write effect
//   if it's applied to a parameter of a method, makes that parameter a write effect
case class write() extends EffectAnnotation
// for a method
case class global() extends EffectAnnotation
