package ch.epfl.yinyang

import scala.reflect.macros.Context

trait YYConfig {
  val config: Map[String, Any]

  val shallow: Boolean = config("shallow").asInstanceOf[Boolean]
  val debug: Int = config("debug").asInstanceOf[Int]
  val mainMethod: String = config("mainMethod").asInstanceOf[String]
  val featureAnalysing: Boolean = config("featureAnalysing").asInstanceOf[Boolean]
  val ascriptionTransforming: Boolean = config("ascriptionTransforming").asInstanceOf[Boolean]
  val liftTypes: List[Context#Type] = config("liftTypes").asInstanceOf[List[Context#Type]] // SI-7234  prevents us from having it as a field to YYTransformers
}