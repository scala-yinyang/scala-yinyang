package ch.epfl.yinyang
package api

import reflect.runtime.universe._

/**
 * Base trait for code generating DSLs.
 */
trait CodeGenerator { this: BaseYinYang =>

  /**
   * Should return a string with a class definition of the DSL code. The class
   * must be a Scala FunctionX with adequate number of types of arguments.
   * @param className The name of the class.
   * @param stableMixed If the DSL program has optional compilation variables,
   *   this set contains the holeIds of those that should be treated as stable,
   *   i.e. like required variables. For the other (unstable) optional
   *   variables, generic code using only the hole and not the value of the
   *   mixed node should be generated.
   */
  def generateCode(className: String, stableMixed: Set[Int] = Set()): String

  /**
   * Method that should compile the DSL and return a function of type (...) => T.
   * @param stableMixed The set of optional variables that should be treated as
   * stable, to be passed to the `generateCode` function.
   */
  def compile[T: TypeTag, Ret](stableMixed: Set[Int] = Set()): Ret
}