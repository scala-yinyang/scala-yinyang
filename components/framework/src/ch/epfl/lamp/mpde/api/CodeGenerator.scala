package ch.epfl.lamp.mpde.api

trait CodeGenerator {

  def generateCode: String

  /**
   * Should be used in compile-time code generation to replace free variables.
   *
   * TODO Should we provide the hole evidence like we do with literals?
   * Probably not.
   */
  def hole[T](varName: String): T
}