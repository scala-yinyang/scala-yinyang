package ch.epfl.lamp
package autolifter
package types

import scala.reflect.api.Universe

trait UniverseContainer {
  val universe: Universe
}