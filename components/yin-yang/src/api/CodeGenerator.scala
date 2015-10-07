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

trait Compiled { self: BaseYinYang =>
  /**
   * Method that should compile the DSL and return a function of type (...) => T.
   * @param unstableHoleIds The set of optional variables that should be treated as
   *   unstable, to be passed to the `generateCode` function.
   */
  def compile[T: TypeRep, Ret](): Ret
}

trait Executeable { self: BaseYinYang =>
  def execute[T](args: Seq[Any]): T
}

