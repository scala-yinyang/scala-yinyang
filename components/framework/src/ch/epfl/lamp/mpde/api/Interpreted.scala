package ch.epfl.lamp.yinyang
package api

trait Interpreted {
  def interpret[T: Manifest](params: Any*): T
}
