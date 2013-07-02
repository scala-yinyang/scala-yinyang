package ch.epfl.yinyang.api

import reflect.runtime.universe.Symbol

/**
 * Marker trait for DSLs in which no hole is treated as constant and all holes are needed to be recomputed.
 *
 * NOTE: DSLs that inherit this trait will not be reflectively instantiated at compile time.
 */
trait FullyUnstaged { this: BaseYinYang =>

  override def requiredHoles(symbols: List[Symbol]): List[Int] =
    throw new RuntimeException("This method must not be called!!!")

}
