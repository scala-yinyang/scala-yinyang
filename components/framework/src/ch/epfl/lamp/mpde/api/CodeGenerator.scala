package ch.epfl.lamp.mpde
package api

import ch.epfl.lamp.mpde.MPDETransformer
import scala.reflect.macros.Context

trait BaseYinYang {

  def stagingAnalyze(): List[Long]

  def hole[T](symbolId: Int): T

}

trait CodeGenerator {

  def generateCode(className: String): String

  def generateName(id: Int): String = s"generated$$bridge$$$id"

  //def main(): Any

}
