package ch.epfl.lamp.mpde
package api

trait CodeGenerator {

  def generateCode(className: String): String

  def compile[T: Manifest, Ret]: Ret

}
