package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for code generating DSLs.
 */
trait CodeGenerator { this: BaseYinYang =>

  /**
   * Should return a string with a class definition of the DSL code. The class must
   * be a Scala FunctionX with adequate number of types of arguments.
   */
  def generateCode(className: String): String

  /**
   * Method that should compile the DSL and return a function of type (...) => T.
   */
  def compile[T: TypeTag: ClassTag, Ret]: Ret
}