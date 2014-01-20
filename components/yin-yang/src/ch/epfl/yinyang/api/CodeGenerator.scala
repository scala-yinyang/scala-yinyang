package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for code generating DSLs.
 */
trait CodeGenerator { this: BaseYinYang =>

  /**
   * Should return a string with a class definition of the DSL code. The class
   * must be a Scala FunctionX with adequate number of types of arguments.
   * @param className The name of the class.
   * @param unstableHoleIds If the DSL program has optional compilation
   *   variables, this set contains the holeIds of the variables for which
   *   the value should be ignored and generic code using only the hole should
   *   be generated. The others (stable) should be treated like required
   *   variables.
   */
  def generateCode(className: String, unstableHoleIds: Set[Int] = Set()): String

  /**
   * Method that should compile the DSL and return a function of type (...) => T.
   * @param unstableHoleIds The set of optional variables that should be treated as
   *   unstable, to be passed to the `generateCode` function.
   */
  def compile[T: TypeTag, Ret](unstableHoleIds: Set[Int] = Set()): Ret
}