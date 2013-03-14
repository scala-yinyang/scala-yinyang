package ch.epfl.lamp.mpde
package api

trait Interpreted {

  def interpret[T: Manifest](params: Any*): T

}
