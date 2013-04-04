package ch.epfl.lamp.yinyang
package api

trait Interpreted {
  def reset(): Unit
  def interpret[T: Manifest](params: Any*): T
}
