package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for code generating DSLs.
 */
trait CodeGenerator { self: BaseYinYang =>

  /**
   * Should return a string with a class definition of the DSL code. The class
   * must be a Scala FunctionX with adequate number of types of arguments.
   * @param className The name of the class.
   */
  def generateCode(className: String): String

}
