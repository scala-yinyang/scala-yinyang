package ch.epfl.yinyang

trait YYConfig {
  val config: Map[String, Any]

  val shallow: Boolean = config("shallow").asInstanceOf[Boolean]
  val debug: Int = config("debug").asInstanceOf[Int]
  val mainMethod: String = config("mainMethod").asInstanceOf[String]
  val featureAnalysing: Boolean = config("featureAnalysing").asInstanceOf[Boolean]
  val ascriptionTransforming: Boolean = config("ascriptionTransforming").asInstanceOf[Boolean]
}