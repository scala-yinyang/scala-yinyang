package ch.epfl.yinyang
package api

import reflect.ClassTag
import reflect.runtime.universe.TypeTag

/**
 * Base trait for DSLs that will be used for lifting DSLs into an IR.
 * The DSL component will be called with a method:
 *  {{{
 *    val dsl = // shallow DSL of type Ret translated by Yin-Yang
 *    dsl.reify(runtimeType[Ret])
 *  }}}
 */
trait Reified { self: BaseYinYang => }
