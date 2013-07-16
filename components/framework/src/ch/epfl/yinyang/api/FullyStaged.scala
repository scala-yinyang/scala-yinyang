package ch.epfl.yinyang.api

import reflect.runtime.universe.Symbol

/**
 * Marker trait for DSLs in which all holes are treated as constants.
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated at compile time.
 */
trait FullyStaged { this: BaseYinYang =>

  override def requiredHoles(symbols: List[Symbol]): List[Symbol] =
    throw new RuntimeException("This method must not be called!!!")

}
