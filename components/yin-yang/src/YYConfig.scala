package ch.epfl.yinyang

import scala.reflect.macros.blackbox.Context

trait YYConfig {
  val config: Map[String, Any]

  val shallow: Boolean = config("shallow").asInstanceOf[Boolean]
  val debug: Int = config("debug").asInstanceOf[Int]
  val shortenDSLNames: Boolean = config("shortenDSLNames").asInstanceOf[Boolean]
  val mainMethod: String = config("mainMethod").asInstanceOf[String]
  val featureAnalysing: Boolean = config("featureAnalysing").asInstanceOf[Boolean]
  val ascriptionTransforming: Boolean = config("ascriptionTransforming").asInstanceOf[Boolean]
  val liftTypes: List[Context#Type] = config("liftTypes").asInstanceOf[List[Context#Type]] // SI-7234  prevents us from having it as a field to YYTransformers
  val optionalInitiallyStable: Boolean = config("optionalInitiallyStable").asInstanceOf[Boolean]
  val codeCacheSize: Int = config("codeCacheSize").asInstanceOf[Int]
  val minimumCountToStabilize: Int = config("minimumCountToStabilize").asInstanceOf[Int]
  val virtualizeFunctions: Boolean = config("virtualizeFunctions").asInstanceOf[Boolean]
  val virtualizeVal: Boolean = config("virtualizeVal").asInstanceOf[Boolean]
  val failCompilation: Boolean = config("failCompilation").asInstanceOf[Boolean]
}