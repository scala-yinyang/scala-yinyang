package ch.epfl.lamp.mpde
package api

import ch.epfl.lamp.mpde.MPDETransformer
import scala.reflect.macros.Context

trait BaseYinYang {

  def manifest[T]()(implicit m: Manifest[T]) = m

  def stagingAnalyze(): List[Int]

  trait HoleEvidence[T, Ret] {
    def emit(tpe: Manifest[Any], symbolId: scala.Int): Ret
  }

  def hole[T, Ret](tpe: Manifest[T], symbolId: scala.Int)(implicit holeEv: HoleEvidence[T, Ret]): Ret =
    holeEv emit (tpe.asInstanceOf[Manifest[Any]], symbolId)

}

trait CodeGenerator {

  def generateCode(className: String): String

  def compile[T: Manifest, Ret]: Ret

  //def main(): Any

}
