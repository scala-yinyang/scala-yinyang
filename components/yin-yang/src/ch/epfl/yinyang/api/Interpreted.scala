package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for interpreted DSLs.
 */
trait Interpreted { this: BaseYinYang =>

  /**
   * Resets the internal state of the DSL.
   */
  def reset(): Unit

  /**
   * Accepts the captured values and returns the result.
   */
  def interpret[T: TypeRep](params: Any*): T
}