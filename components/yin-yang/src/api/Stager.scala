package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for interpreted DSLs.
 */
trait Stager { this: BaseYinYang =>

  def stage[T](): T
}