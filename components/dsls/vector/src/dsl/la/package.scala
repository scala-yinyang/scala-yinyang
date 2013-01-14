package dsl

import ch.epfl.lamp.mpde.MPDE._
import scala.language.experimental.macros

package object la {
  
  def la[T](body: => T) = body
  
  // TODO should not return Unit but a value
  def laLifted[T](cake: String, block: => T): Unit = macro lift[T]
  
}