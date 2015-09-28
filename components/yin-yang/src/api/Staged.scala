package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for program DSLs.
 */
trait Staged { this: BaseYinYang =>

  def stage[T](): T

}
