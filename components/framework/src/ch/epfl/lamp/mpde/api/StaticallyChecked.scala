package ch.epfl.lamp.yinyang
package api
import scala.reflect.macros.Context

trait StaticallyChecked {
  def staticallyCheck(c: Context)
}