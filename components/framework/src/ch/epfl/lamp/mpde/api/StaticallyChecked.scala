package ch.epfl.lamp.mpde.api

import scala.reflect.macros.Context

trait StaticallyChecked {
  def staticallyCheck(c: Context)
}