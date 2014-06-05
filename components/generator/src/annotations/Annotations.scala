package ch.epfl.lamp
package autolifter
package annotations

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{ universe => ru }

final case class Custom(methods: List[CMethod] = Nil, parentLevel: Int = 1) extends StaticAnnotation

object Custom {
  implicit val defaultCustom: Custom = Custom()
}

final case class CMethod(name: String, paramss: List[List[CType]] = Nil, effect: types.Effect = types.Pure) extends StaticAnnotation
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