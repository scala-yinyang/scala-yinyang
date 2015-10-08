package ch.epfl.yinyang

import scala.reflect.macros.blackbox.Context

trait YYConfig {
  val config: Map[String, Any]
  val direct: Boolean = config("direct").asInstanceOf[Boolean]
  val debug: Int = config("debug").asInstanceOf[Int]
  val shortenDSLNames: Boolean = config("shortenDSLNames").asInstanceOf[Boolean]
  val mainMethodName: String = config("mainMethodName").asInstanceOf[String]
  val restrictLanguage: Boolean = config("restrictLanguage").asInstanceOf[Boolean]
  val ascribeTerms: Boolean = config("ascribeTerms").asInstanceOf[Boolean]
  val liftTypes: List[Context#Type] = config("liftTypes").asInstanceOf[List[Context#Type]] // SI-7234  prevents us from having it as a field to YYTransformers
  val virtualizeFunctions: Boolean = config("virtualizeFunctions").asInstanceOf[Boolean]
  val virtualizeValDef: Boolean = config("virtualizeValDef").asInstanceOf[Boolean]
  val failCompilation: Boolean = config("failCompilation").asInstanceOf[Boolean]
}
